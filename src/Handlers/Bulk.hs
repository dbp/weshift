{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Handlers.Bulk where

-- | Boilerplate imports
import qualified Data.Bson             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Imports
import qualified Text.XmlHtml          as X
import qualified Utils                 as U

-- | Module specific imports
import           Data.Char             (digitToInt, isSpace)
import           Data.Either           (lefts, rights)
import           Text.Parsec
import           Text.SSV

import           State.Coworkers
import           State.Shifts

import           Render.Bulk
import           Render.Shifts

bulkInputH :: User -> UserPlace -> AppHandler ()
bulkInputH user place =
  route [("/", ifTop $ method GET $ inputForm user)
        ,("/upload", method POST $ upload user place)
        ,("/confirm", method POST $ confirm user place)
        ]

inputForm u = do
    today <- fmap utctDay $ liftIO getCurrentTime
    setView u "work" "work.bulk"
    heistLocal (bindSplices (commonSplices today)) $ renderWS "work/bulk"

upload u p = do
  md <- getParam "data"
  case md of
    Nothing -> redirPlaceHomeAsync
    Just d -> do
      workers <- if pFac p then getWorkers p else return [u] -- if they are not a facilitator, they can only load their own shifts
      let (understood, notUnderstood) = parseShifts workers $ transformCSV $ readCSV (B8.unpack d)
      today <- liftM utctDay $ liftIO getCurrentTime
      heistLocal (bindSplices $ do "understood" ## renderShifts understood
                                   "understood-serialized" ## textSplice $ TE.decodeUtf8 $ urlEncode $ B8.pack $ show understood
                                   "not-understood" ## renderNotUnderstood notUnderstood
                                   "not-understood-serialized" ## textSplice $ TE.decodeUtf8 $ urlEncode $ B8.pack $ show notUnderstood
                                   commonSplices today)
                $ renderWS "work/bulk_understood"

confirm u p = do
  mshifts <- getParam "understood"
  n <- getParam "notunderstood"
  case (mshifts >>= urlDecode >>= (maybeRead . TE.decodeUtf8), n >>= urlDecode >>= (maybeRead . TE.decodeUtf8)) of
    (Just shifts, Just notUnderstood) -> do
      today <- liftM utctDay $ liftIO getCurrentTime
      workers <- getWorkers p
      res <- mapM insertShift $ filterFac $ map (\s -> s { sPlace = (pId p), sRecorder = (uId u) }) shifts
      let numSuccess = length $ rights res
      let numFailed = length $ lefts res
      let unparsed = renderNotUnderStoodCSV ((mapMaybe (formatNU workers) $ lefts res) ++ notUnderstood)
      let status = if numFailed == 0 then "Inserted all " ++ (show numSuccess) ++ " shifts. Any not understood shifts are below"
                   else "Inserted " ++ (show numSuccess) ++ "/" ++ (show $ numSuccess + numFailed) ++ " shifts. Those that could not be inserted (due to overlapping with existing shifts), as well as those that could not be understood, are below:"
      heistLocal (bindSplices (do "status" ## textSplice $ T.pack status
                                  "data" ## textSplice $ T.pack unparsed
                                  (commonSplices today))) $ renderWS "work/bulk"
    _ -> renderWS "work/bulk_error"

 where filterFac ss = if pFac p then ss else filter (\s -> sUser s == uId u) ss
       formatNU workers (Shift _ user _ start stop _ _ _ _ _ _ _ _) =
          do u <- find ((== user).uId) workers
             return (T.unpack $ uName u, localDay start, showTimeRange start stop)
       showTimeRange start stop = (ftime start) ++ "-" ++ (ftime stop)
       ftime t = if ftime' "%M" t == "00" then ftime' "%H" t else ftime' "%R" t
       ftime' = formatTime defaultTimeLocale

-- | transformCSV makes it so that each cell is a standalone three-tuple that is the name, date, and shift
transformCSV :: [[String]] -> [(String,Day,String)]
transformCSV [] = []
transformCSV csv@(_:_:_) | length headings >= maximum (map length fields) = concatMap transformFields fields
  where headings = (fromGregorian 0 0 0)  : (mapMaybe fuzzyParseDate $ head csv)
        fields = tail csv
        transformFields [] = []
        transformFields (name:shifts) = map (\(s,index) -> (name,headings !! (index + 1), s)) $ zip shifts (iterate (+1) 0)
transformCSV _ = []

parseShifts :: [User] -> [(String,Day,String)] -> ([Shift],[(String,Day,String)])
parseShifts workers fields = (parsed,unparseable)
  where ps = map parseShift fields
        parseShift (person,day,datafield) =
          let timerange = takeWhile (/= '(') datafield
              (D color units desc) = parseMeta (dropWhile (/= '(') datafield)
              rp = do r <- mbRange timerange
                      p <- find ((== person) . T.unpack . uName) workers
                      return (r,p)
          in
          case rp of
            Nothing -> Left (person,day,datafield)
            Just ((s,e), worker) -> Right $ emptyShift {sUser = (uId worker), sStart = (mkTime day s), sStop = (mkTime day e), sUnits = units, sColor = color, sDescription = desc}
        mbRange t = either (const Nothing) Just (parse timeRange "" t)
        mkTime d t = LocalTime d (timeToTimeOfDay t)
        unparseable = lefts ps
        parsed = rights ps


data GD = GC Color | GU Double | GS T.Text
data D = D Color Double T.Text
defaultD = D Transparent 0 ""

-- | parseMeta is looking for (1,C,something) or (1) or (C,something), etc.
parseMeta [] = defaultD
parseMeta (x:[]) = defaultD
parseMeta d = metaGuess $ splitOn ";" $ trim d

metaGuess x = combine $ map guess (map strip x)

guess :: String -> GD
guess "R" = GC Red
guess "B" = GC Blue
guess "G" = GC Green
guess u | isJust (maybeReadS u :: Maybe Double) = GU (fromJust (maybeReadS u))
guess s = GS (T.pack s)

-- | this is a reasonably tollerant way of accepting data. it allows multiple of the same type,
--   and just drops the first ones, but it will limit 3 fields.
combine :: [GD] -> D
combine x | length x > 3 = defaultD -- if they give malformed data, don't even try
combine x = combine' defaultD x
combine' :: D -> [GD] -> D
combine' cur [] = cur
combine' (D oldc u s) ((GC c):xs) = combine' (D c u s) xs
combine' (D c oldu s) ((GU u):xs) = combine' (D c u s) xs
combine' (D c u olds) ((GS s):xs) = combine' (D c u s) xs

-- | trim gets rid of leading and trailing space, and the first and last non-space character
trim s = strip (init (tail (strip s)))

-- | strip gets rid of leading and trailing space
strip s = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
