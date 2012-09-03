{-# LANGUAGE OverloadedStrings #-}

module Common where

import Text.Templating.Heist
import qualified Text.XmlHtml as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Map as M
import Snap.Snaplet.Heist
import Snap.Snaplet
import Data.ByteString (ByteString)
import Snap.Snaplet.Session
import Control.Monad.Trans (lift, liftIO)
import Heist.Splices.Async (heistAsyncSplices)
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, isJust, fromJust, isNothing)
import Control.Monad (liftM, mzero, unless)
import Control.Applicative
import Data.List (find)
import Data.List.Split
import Snap.Core
import Text.Digestive.Heist
import Text.Digestive.Snap hiding (method)
import Text.Digestive

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import System.Locale

import Application 
import Auth
import State.Types
import State.Account
import State.Place
import State.Coworkers


-- | this function is a helper for our use of digestive functors - we always use same prefix
wsForm :: Form v AppHandler a -> AppHandler (View v, Maybe a)
wsForm = runForm "ws"

-- | this function takes a 'key' and a 'value' for the view, and sets it in the user's account.
-- they key is the prefix of the value, so it is stored like: key.value.sub;key2.value2 etc.
-- the idea is that these will indicate which panels are open, so that the site remembers
-- what everything looks like.
setView :: User -> BS.ByteString -> BS.ByteString -> AppHandler ()
setView user key value = do
    updateUserView $ user { uView = view }
    return ()
  where views = T.splitOn ";" $ TE.decodeUtf8 (uView user)
        newViews = map (\v -> if (TE.decodeUtf8 key) `T.isPrefixOf` v then (TE.decodeUtf8 value) else v) views
        view = TE.encodeUtf8 $ T.intercalate ";" newViews
        
-- | get's the suffix of the view that begins with what is provided, if it exists
getView :: User -> T.Text -> Maybe BS.ByteString
getView u i = fmap TE.encodeUtf8 $ (T.stripPrefix i) =<< (find (T.isPrefixOf i) views)
  where views = (T.splitOn ";" $ TE.decodeUtf8 (uView u))


viewSplice :: BS.ByteString -> Splice AppHandler
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
  where views = (T.splitOn ";" $ TE.decodeUtf8 v)
        isView i = i `elem` views
        hasView i = any (T.isPrefixOf i) views
        getView i = map X.TextNode $ maybeToList $ (T.stripPrefix i) =<< (find (T.isPrefixOf i) views)

placeName place = BS.intercalate ", " $ (map ($ place) [pName,pOrg])
placeRoot place = BS.intercalate "/"  $ (map (repUnders. ($ place)) [const "",pOrg,pName])

getCurrentPlace :: AppHandler (Maybe UserPlace)
getCurrentPlace = do mplaceId <- with sess $ getFromSession "place"
                     muser <- getCurrentUser
                     let hm = do placeId <- fmap TE.encodeUtf8 $ mplaceId
                                 user <- muser
                                 place <- find ((==) placeId . pId) $ uPlaces user
                                 return $ place
                     return hm
getCurrentUserAndPlace :: AppHandler (Maybe (User,UserPlace))
getCurrentUserAndPlace = do mplaceId <- with sess $ getFromSession "place"
                            muser <- getCurrentUser
                            let hm = do placeId <- fmap TE.encodeUtf8 $ mplaceId
                                        user <- muser
                                        place <- find ((==) placeId . pId) $ uPlaces user
                                        return $ (user,place)
                            return hm
                     
redirPlaceHome :: AppHandler ()
redirPlaceHome = do hm <- liftM (fmap placeRoot) getCurrentPlace
                    redirect $ fromMaybe "/" hm

redirectAsync url = heistLocal (bindSplice "url" (textSplice $ TE.decodeUtf8 url)) $ renderWS "redirect"

redirPlaceHomeAsync :: AppHandler ()
redirPlaceHomeAsync = do hm <- liftM (fmap placeRoot) getCurrentPlace
                         redirectAsync $ fromMaybe "/" hm


checkPlaceLogin = checkPlaceLogin' redirect
checkPlaceLoginAsync = checkPlaceLogin' redirectAsync

checkPlaceLogin' :: (BS.ByteString -> AppHandler ()) -> Maybe BS.ByteString -> Maybe BS.ByteString -> (User -> UserPlace -> AppHandler ()) -> AppHandler ()
checkPlaceLogin' redr (Just org) (Just place) handler = 
  do u <- getCurrentUser
     uri <- liftM rqURI getRequest
     p <- getPlace (repSpaces org) (repSpaces place)
     let loginPage = maybe (redr "/")
                           (\pl -> redr (BS.concat ["/login?redirectTo=", uri, "&pl=", pId pl]))
                           p
     case (userPlace p u, u) of
       (Just pl, Just u) -> do
         with sess $ setInSession "place" (TE.decodeUtf8 $ pId pl)
         handler u pl
       _ -> loginPage
       
  where userPlace (Just pl) (Just u) = if (uSuper u) then Just (pl {pFac = True}) else find (\p -> pName pl == pName p && pOrg pl == pOrg p) (uPlaces u)
        userPlace _ _ = Nothing
checkPlaceLogin' _ _ _ _ = mzero

renderTime t = T.pack $ formatTime defaultTimeLocale "%-l:%M%P" t
renderDate t = T.pack $ formatTime defaultTimeLocale "%-m.%-d.%Y" t
renderDateLong t = T.pack $ formatTime defaultTimeLocale "%B %e, %Y" t

spliceMBS :: T.Text -> Maybe BS.ByteString -> [(T.Text, Splice AppHandler)]
spliceMBS name val = maybeToList $ fmap (\p -> (name, return [X.TextNode (TE.decodeUtf8 p)])) val

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

parseWSDate s = parseTime defaultTimeLocale "%-m.%-d.%Y" $ B8.unpack s
                         
-- stolen from cgi:
maybeRead :: Read a => ByteString -> Maybe a
maybeRead = fmap fst . listToMaybe . reads . B8.unpack

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


userLookup :: M.Map BS.ByteString User -> Splice AppHandler
userLookup users = do node <- getParamNode
                      case X.getAttribute "id" node >>= (flip M.lookup users . TE.encodeUtf8) of
                        Nothing -> return []
                        Just user -> runChildrenWith [("id", textSplice $ TE.decodeUtf8 $ uId user)
                                                     ,("name", textSplice $ TE.decodeUtf8 $ uName user)
                                                     ,("active", if uActive user then identitySplice else blackHoleSplice)
                                                     ,("super",  if uSuper user then identitySplice else blackHoleSplice)
                                                     ]  

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


commonSplices today = [("currYear",  textSplice $ T.pack $ show year)
                      ,("currMonth", textSplice $ T.pack $ show month)
                      ]
  where (year,month,_) = toGregorian today  

renderPlaces :: Monad m => Maybe UserPlace -> [UserPlace] -> Splice m
renderPlaces curPlace places =
      mapSplices (\p -> runChildrenWithText [("id", TE.decodeUtf8 (pId p))
                                            ,("name", TE.decodeUtf8 (pName p))
                                            ,("org", TE.decodeUtf8 (pOrg p))
                                            ,("shortname", TE.decodeUtf8 $ shorten $ BS.concat [pName p,", ",pOrg p])
                                            ,("root", TE.decodeUtf8 (placeRoot p))
                                            ,("current", if (Just p) == curPlace then "selected" else "notselected")
                                            ])
                  places
    where shorten s = if (BS.length s > 20) then (BS.append (BS.take 17 s) "...") else s

renderWS :: ByteString -> AppHandler ()
renderWS t = do mup <- getCurrentUserAndPlace
                let userSplices = do (u,p) <- mup
                                     return [("placeRoot", bTS $ placeRoot p)
                                            ,("placeName", bTS $ placeName p)
                                            ,("userName", bTS $ uName u)
                                            ,("userId", bTS $ uId u)
                                            ,("view", viewSplice (uView u))
                                            ,("userPlaces", renderPlaces (Just p) (uPlaces u))
                                            ]
                let permissionSplices = [("isFacilitator", facilitatorSplice $ fmap snd mup)
                                        ,("isNormalUser", normalUserSplice $ fmap snd mup)
                                        ,("ifLoggedIn", booleanSplice (isJust mup))
                                        ,("ifGuest", booleanSplice (isNothing mup))
                                        ]
                workersSplice <- case mup of
                                    Just (cu, p) -> do 
                                      us <- getWorkers p
                                      return [("user-lookup", userLookup $ M.fromList $ map (\u -> (uId u, u)) us)
                                             ,("allUsers", mapSplices (\w -> runChildrenWith 
                                                 [("id", textSplice $ TE.decodeUtf8 (uId w))
                                                 ,("name", textSplice $ TE.decodeUtf8 (uName w))
                                                 ,("current", if (uId w) == (uId cu) then identitySplice else blackHoleSplice)
                                                 ,("not-current", if (uId w) == (uId cu) then blackHoleSplice else identitySplice)
                                                 ]) us)
                                             ]
                                    Nothing -> return []

                (heistLocal $ (bindSplices (concat [fromMaybe [] userSplices
                                                   ,permissionSplices
                                                   ,workersSplice
                                                   ,heistAsyncSplices
                                                   ,[("show", showContent)]
                                                   ]))) $ render t
  where bTS = textSplice . TE.decodeUtf8
                  
                  
redirTo :: AppHandler ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r