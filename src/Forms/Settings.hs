{-# LANGUAGE OverloadedStrings #-}

module Forms.Settings where

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

data NewPassword = NewPassword Text Text Text deriving (Eq,Show)

checkPassword = checkM "Current password not correct:" fn
  where fn p = do mUs <- getCurrentUser
                  case mUs of
                    Nothing -> return False
                    Just u ->
                      fmap (not.null) $ withPGDB 
                        "SELECT id FROM users WHERE id = ? AND password = crypt(?, password) LIMIT 1;" 
                        [toSql (uId u), toSql p]   
passwordForm = mkNP
    <$> "current" .: checkPassword (text Nothing)
    <*> newPasswordForm
  where mkNP c (n,p) = NewPassword c n p
nameForm mname = "name" .: nonEmpty (text mname)

-- | regex validation sucks, so don't even try.
validEmail = check "Must be a valid email, like help@weshift.org" $ \e -> let s = TE.encodeUtf8 e in '@' `B8.elem` s && '.' `B8.elem` s

emailForm :: Form Text AppHandler Text
emailForm = "address" .: validEmail (text Nothing)
