{-# LANGUAGE OverloadedStrings #-}

module Forms.Coworkers where

-- | Boilerplate imports
import            Imports
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as TE
import qualified  Data.Bson as B
import qualified  Data.Map as M
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Text.XmlHtml as X
import qualified  Utils as U

-- | Module specific imports
import State.Account
import Forms.Settings

userNotIn place = checkM "Another user with this name already exists at this place." $ \n -> fmap not $ userExists place n

newUserForm :: UserPlace -> Form Text AppHandler Text
newUserForm p = userNotIn p nameForm

validEmailOrBlank = check "Must be a valid email, like help@weshift.org" $ \e -> let s = TE.encodeUtf8 e in ('@' `B8.elem` s && '.' `B8.elem` s) || (T.null e)

emailOrBlankForm :: Form Text AppHandler Text
emailOrBlankForm = "email" .: validEmailOrBlank (text Nothing)
