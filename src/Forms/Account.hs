{-# LANGUAGE OverloadedStrings #-}

module Forms.Account where

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
import           State.Place


data SignupCreds = SignupCreds Text Text Text

uniquePlace = checkM "Place with this name and organization already exists" $
                    \(SignupCreds o p _) -> fmap not $ placeExists o p

validOrg mu = case mu of
                Nothing -> checkM "Must be logged in as a facilitator to add to existing organizations" $
                                  \o -> fmap not $ organizationExists o
                Just u -> checkM "Must be a facilitator for this organization to add a new place" $
                                  \o -> do exists <- organizationExists o
                                           case exists of
                                             True -> return $ o `elem` (map pOrg $ filter pFac (uPlaces u))
                                             False -> return True

--signupForm :: Maybe User -> SnapForm AppHandler Text HeistView SignupCreds
signupForm mu = uniquePlace $ SignupCreds
    <$> "organization" .: (validOrg mu $ nonEmpty (text Nothing))
    <*> "place" .: nonEmpty (text Nothing)
    <*> "name" .: (nonEmptyIfNothing mu (text Nothing))
