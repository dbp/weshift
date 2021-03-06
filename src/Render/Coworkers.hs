{-# LANGUAGE OverloadedStrings #-}

module Render.Coworkers where

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

-- | Module Specific imports
import           State.Account         (getActivationLink)

-- | important: the place that is passed is the place of the logged in user, so it's "pFac" will not be relevant.
renderCoworker :: User -> Splice AppHandler
renderCoworker u@(User uid uname uact usuper uplaces uview utoken) = do
  let place = head uplaces
  runChildrenWith $ do "id" ##        textSplice uid
                       "name" ##      textSplice uname
                       "isSuper" ##  booleanSplice  usuper
                       "notSuper" ##  booleanSplice $ not usuper
                       "inActive" ##  booleanSplice $ not uact
                       "activationLink" ##activationLink u
                       "classes" ##   textSplice $ T.concat (["member"] ++ if pFac place then [" facilitator"] else [])
                       "places" ##    renderPlaces Nothing uplaces
                       "fac" ##       if pFac place then identitySplice else blackHoleSplice
                       "notfac" ##    if pFac place then blackHoleSplice else identitySplice


activationLink :: User -> Splice AppHandler
activationLink u = do
    lnk <- lift $ getActivationLink u
    case lnk of
      Nothing -> return []
      Just link -> return [X.TextNode link]

renderCoworkers :: [User] -> Splice AppHandler
renderCoworkers coworkers = mapSplices renderCoworker coworkers



coworkersSplice cs = do "coworkersCount" ## textSplice $ T.pack $ show $ length cs
                        "coworkers" ## renderCoworkers cs
                        "name-errors" ## blackHoleSplice
