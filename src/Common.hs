{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Control.Applicative
import           Control.Monad         (liftM, mzero, unless)
import           Control.Monad.Trans   (lift, liftIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import           Data.List             (find)
import           Data.List.Split
import qualified Data.Map              as M
import           Data.Maybe            (fromJust, fromMaybe, isJust, isNothing,
                                        listToMaybe, maybeToList)
import           Data.Monoid
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Heist                 hiding (Error)
import           Heist.Interpreted
import           Heist.Splices.Async   (heistAsyncSplices)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap   hiding (method)
import qualified Text.XmlHtml          as X

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale

import           Application
import           Auth
import           State.Account
import           State.Coworkers
import           State.Place
import           State.Shifts
import           State.Types


-- | this function is a helper for our use of digestive functors - we always use same prefix
wsForm :: Form v AppHandler a -> AppHandler (View v, Maybe a)
wsForm = runForm "ws"

-- | this function takes a 'key' and a 'value' for the view, and sets it in the user's account.
-- they key is the prefix of the value, so it is stored like: key.value.sub;key2.value2 etc.
-- the idea is that these will indicate which panels are open, so that the site remembers
-- what everything looks like.
setView :: User -> T.Text -> T.Text -> AppHandler ()
setView user key value = do
    updateUserView $ user { uView = view }
    return ()
  where views = T.splitOn ";" $ uView user
        newViews = map (\v -> if key `T.isPrefixOf` v then value else v) views
        view = T.intercalate ";" newViews

-- | get's the suffix of the view that begins with what is provided, if it exists
getView :: User -> T.Text -> Maybe T.Text
getView u i = (T.stripPrefix i) =<< (find (T.isPrefixOf i) views)
  where views = (T.splitOn ";" $ uView u)


viewSplice :: T.Text -> Splice AppHandler
viewSplice v = do node <- getParamNode
                  case X.getAttribute "is" node of
                    Just i -> if isView i then return (X.elementChildren node) else return []
                    Nothing ->
                      case X.getAttribute "has" node of
                        Just i -> if hasView i then return (X.elementChildren node) else return []
                        Nothing ->
                          case X.getAttribute "val" node of
                            Just i -> return (getView i)
                            Nothing -> return []
  where views = (T.splitOn ";" v)
        isView i = i `elem` views
        hasView i = any (T.isPrefixOf i) views
        getView i = map X.TextNode $ maybeToList $ (T.stripPrefix i) =<< (find (T.isPrefixOf i) views)

placeName place = T.intercalate ", " $ (map ($ place) [pName,pOrg])
placeRoot place = T.intercalate "/"  $ (map (repUnders. ($ place)) [const "",pOrg,pName])

getCurrentPlace :: AppHandler (Maybe UserPlace)
getCurrentPlace = do mplaceId <- with sess $ getFromSession "place"
                     muser <- getCurrentUser
                     let hm = do placeId <- mplaceId
                                 user <- muser
                                 place <- find ((==) placeId . pId) $ uPlaces user
                                 return $ place
                     return hm
getCurrentUserAndPlace :: AppHandler (Maybe (User,UserPlace))
getCurrentUserAndPlace = do mplaceId <- with sess $ getFromSession "place"
                            muser <- getCurrentUser
                            let hm = do placeId <- mplaceId
                                        user <- muser
                                        place <- find ((==) placeId . pId) $ uPlaces user
                                        return $ (user,place)
                            return hm

redirPlaceHome :: AppHandler ()
redirPlaceHome = do hm <- liftM (fmap placeRoot) getCurrentPlace
                    redirect $ maybe "/" TE.encodeUtf8 hm

redirectAsync url = heistLocal (bindSplice "url" (textSplice url)) $ renderWS "redirect"

redirPlaceHomeAsync :: AppHandler ()
redirPlaceHomeAsync = do hm <- liftM (fmap placeRoot) getCurrentPlace
                         redirectAsync $ fromMaybe "/" hm


checkPlaceLogin = checkPlaceLogin' (redirect . TE.encodeUtf8)
checkPlaceLoginAsync = checkPlaceLogin' redirectAsync

checkPlaceLogin' :: (T.Text -> AppHandler ()) -> Maybe T.Text -> Maybe T.Text -> (User -> UserPlace -> AppHandler ()) -> AppHandler ()
checkPlaceLogin' redr (Just org) (Just place) handler =
  do u <- getCurrentUser
     uri <- liftM (TE.decodeUtf8 . rqURI) getRequest
     p <- getPlace (repSpaces org) (repSpaces place)
     let loginPage = maybe (redr "/")
                           (\pl -> redr (T.concat ["/login?redirectTo=", uri, "&pl=", pId pl]))
                           p
     case (userPlace p u, u) of
       (Just pl, Just u) -> do
         with sess $ setInSession "place" (pId pl)
         handler u pl
       _ -> loginPage

  where userPlace (Just pl) (Just u) = if (uSuper u) then Just (pl {pFac = True}) else find (\p -> pName pl == pName p && pOrg pl == pOrg p) (uPlaces u)
        userPlace _ _ = Nothing
checkPlaceLogin' _ _ _ _ = mzero

renderTime t = T.pack $ formatTime defaultTimeLocale "%-l:%M%P" t
renderDate t = T.pack $ formatTime defaultTimeLocale "%-m.%-d.%Y" t
renderDateLong t = T.pack $ formatTime defaultTimeLocale "%B %e, %Y" t

spliceMBS :: T.Text -> Maybe T.Text -> Splices (Splice AppHandler)
spliceMBS name val = fromMaybe mempty $ fmap (\p -> (name ## return [X.TextNode p])) val

facilitatorSplice p = do node <- getParamNode
                         case p of
                           Just place -> if (pFac place) then return (X.elementChildren node) else return []
                           Nothing -> return []

normalUserSplice :: Maybe UserPlace -> Splice AppHandler
normalUserSplice p = do node <- getParamNode
                        case p of
                          Just place -> if (not $ pFac place) then return (X.elementChildren node) else return []
                          Nothing -> return []


identitySplice :: Monad m => Splice m
identitySplice = do node <- getParamNode
                    return (X.elementChildren node)

blackHoleSplice :: Monad m => Splice m
blackHoleSplice = return []

booleanSplice :: Monad m => Bool -> Splice m
booleanSplice b = if b then identitySplice else blackHoleSplice

parseWSDate s = parseTime defaultTimeLocale "%-m.%-d.%Y" $ T.unpack s

-- stolen from cgi:
maybeRead :: Read a => T.Text -> Maybe a
maybeRead = maybeReadS . T.unpack
maybeReadS :: Read a => String -> Maybe a
maybeReadS = fmap fst . listToMaybe . reads

nonEmpty :: Form T.Text AppHandler T.Text -> Form T.Text AppHandler T.Text
nonEmpty = check "Field must not be empty:" $ \s -> not $ T.null s

--nonEmptyIfNothing :: Maybe a -> Validator AppHandler T.Text String
nonEmptyIfNothing m = check "Field must not be empty:" $ \s -> if isNothing m then (not $ T.null s) else True

--newPasswordForm  :: SnapForm AppHandler T.Text HeistView (String,String)
newPasswordForm = validate matchingPasswords $ (,)
    <$> "new"     .: nonEmpty (text Nothing)
    <*> "confirm" .: nonEmpty (text Nothing)
  where matchingPasswords (a,b) | a == b = Success (a,b)
        matchingPasswords _ = Error "New passwords do not match:"


userLookup :: M.Map T.Text User -> Splice AppHandler
userLookup users = do node <- getParamNode
                      case X.getAttribute "id" node >>= (flip M.lookup users) of
                        Nothing -> return []
                        Just user -> runChildrenWith $
                                     do "id" ## textSplice $ uId user
                                        "name" ## textSplice $ uName user
                                        "active" ## if uActive user then identitySplice else blackHoleSplice
                                        "super" ##  if uSuper user then identitySplice else blackHoleSplice

shiftClaims :: User -> Splice AppHandler
shiftClaims u = do node <- getParamNode
                   case X.getAttribute "id" node of
                      Nothing -> return []
                      Just shiftId -> do
                        claims <- lift $ getShiftClaims shiftId
                        mapSplices
                          (\(Claim id' shift user units reason resolved accepted) ->
                            runChildrenWith $ do "id" ## textSplice id'
                                                 "shift" ## textSplice shift
                                                 "user" ## textSplice user
                                                 "units" ## textSplice $ T.pack $ show units
                                                 "reason" ## textSplice reason
                                                 "resolved" ## booleanSplice resolved
                                                 "notResolved" ## booleanSplice $ not resolved
                                                 "accepted" ## booleanSplice accepted
                                                 "notAccepted" ## booleanSplice $ not accepted
                                                 "userClaim" ## booleanSplice $ user == (uId u))

                          claims

-- | this splice shows it's children if the blank attribute is blank, or if it's notblank attribute is not blank,
--   or another attribute is equal to it's name
showContent :: Monad m => Splice m
showContent = do node <- getParamNode
                 case X.getAttribute "notblank" node of
                   Just "" -> return []
                   Nothing -> case X.getAttribute "blank" node of
                     Just "" -> return $ X.elementChildren node
                     Nothing -> if checkAttrs node then return (X.elementChildren node) else return []
                     _ -> return []
                   _ -> return $ X.elementChildren node
        where checkAttrs node = any (\(a,b) -> a == b) $ X.elementAttrs node


commonSplices today = do "currYear" ##  textSplice $ T.pack $ show year
                         "currMonth" ## textSplice $ T.pack $ show month
  where (year,month,_) = toGregorian today

renderPlaces :: Monad m => Maybe UserPlace -> [UserPlace] -> Splice m
renderPlaces curPlace places =
      mapSplices (\p -> runChildrenWithText $ do "id" ##  (pId p)
                                                 "name" ##  (pName p)
                                                 "org" ##  (pOrg p)
                                                 "shortname" ##  shorten $ T.concat [pName p,", ",pOrg p]
                                                 "root" ##  (placeRoot p)
                                                 "current" ##  if (Just p) == curPlace then "selected" else "notselected"
                                                 )
                  places
    where shorten s = if (T.length s > 20) then (T.append (T.take 17 s) "...") else s

renderWS :: ByteString -> AppHandler ()
renderWS t = do mup <- getCurrentUserAndPlace
                let userSplices = do (u,p) <- mup
                                     return (do "placeRoot" ## bTS $ placeRoot p
                                                "placeName" ## bTS $ placeName p
                                                "placeId" ## bTS $ pId p
                                                "userName" ## bTS $ uName u
                                                "userId" ## bTS $ uId u
                                                "view" ## viewSplice (uView u)
                                                "userPlaces" ## renderPlaces (Just p) (uPlaces u)
                                                "claims" ## shiftClaims u
                                             )
                let permissionSplices = do "isFacilitator" ## facilitatorSplice $ fmap snd mup
                                           "isNormalUser" ## normalUserSplice $ fmap snd mup
                                           "ifLoggedIn" ## booleanSplice (isJust mup)
                                           "ifGuest" ## booleanSplice (isNothing mup)

                workersSplice <- case mup of
                                    Just (cu, p) -> do
                                      us <- getWorkers p
                                      return $ do "user-lookup" ## userLookup $ M.fromList $ map (\u -> (uId u, u)) us
                                                  "allUsers" ## mapSplices (\w -> runChildrenWith
                                                    (do "id" ## textSplice (uId w)
                                                        "name" ## textSplice $ (uName w)
                                                        "current" ## if (uId w) == (uId cu) then identitySplice else blackHoleSplice
                                                        "not-current" ## if (uId w) == (uId cu) then blackHoleSplice else identitySplice
                                                     )) us
                                    Nothing -> return mempty

                (heistLocal $ (bindSplices (mconcat [fromMaybe mempty userSplices
                                                    ,permissionSplices
                                                    ,workersSplice
                                                    ,heistAsyncSplices
                                                    ,"show" ## showContent
                                                    ]))) $ render t
  where bTS = textSplice


redirTo :: AppHandler ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r
