{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Mail where

import           Control.Monad
import           "mtl" Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text  as A
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Network.HTTP          hiding (getRequest, urlEncode)
import           Network.HTTP.Base     hiding (urlEncode)
import           Network.HTTP.Headers
import           Network.URI           (parseURI)
import           Secrets               (postmarkToken)

import           Snap.Core             hiding (POST)

import           Network.Mail.Postmark

import           State.Types           (Shift (..), User (uName),
                                        UserPlace (pName))
import           Utils                 (wsFormatDay, wsFormatTime)

mailAccountActivation name email link = do
  liftIO $ postmark (B8.unpack postmarkToken) "messages@weshift.org" (T.concat ["Please activate your account with WeShift, ",name,"."]) "activate" (T.concat $ msg link name) email
    where msg l n = [ "Welcome to WeShift, "
                    ,n
                    ,".\n\n"
                    , "To activate your account, please visit "
                    , l
                    , " .\n\nThanks! - The WeShift Team"
                    ]


mailEmailActivation account email token place = do
  server  <- liftM (TE.decodeUtf8 . rqServerName) getRequest
  portNum <- liftM rqServerPort getRequest
  liftIO $ postmark (B8.unpack postmarkToken) "messages@weshift.org" "Please confirm your email with WeShift." "confirm" (T.concat $ msg server portNum token place) email
    where msg s p t pl = [ "Please confirm your email account on WeShift. "
                        , "To do that, visit "
                        , "http://"
                        , s
                        , if p /= 80 then (T.pack $ ':' : (show p)) else ""
                        , "/activate/email?account="
                        , account
                        , "&token="
                        , t
                        , "&pl="
                        , pl
                        , " .\n\nThanks! - The WeShift Team"
                        ]

mailDisabling account email name token place = do
  server  <- liftM (TE.decodeUtf8 . rqServerName) getRequest
  portNum <- liftM rqServerPort getRequest
  liftIO $ postmark (B8.unpack postmarkToken) "messages@weshift.org" "Your account on WeShift has been disabled - important info." "disable" (T.concat $ msg server portNum name token place) email
    where msg s p n t pl = [ "We have deleted all personally identifiable information (any email addresses you registered, and your name),  "
                        , "but due to the interconnected nature of the workplaces on WeShift, we cannot entirely delete a single account."
                        , "\n\nIf all the members of a workplace decide that they want their info permanently erased, have all users similarly disable their accounts, and then send an email to help@weshift.org and we'll be happy to do it."
                        , "\n\nBecause your account still exists (though without anything, aside from what is in this email, that ties it to YOU), we provide a way for you to re-enable it. Please save this email in case you want to do this - we cannot send you this message again, because we have already deleted the record of this email address from our files (hence, this email is the only thing that connects you to your account - which we think is a good thing).\n\nTo re-enable your account, visit:\n\n"
                        , "http://"
                        , s
                        , if p /= 80 then (T.pack $ ':' : (show p)) else ""
                        , "/activate/disabled?account="
                        , account
                        , "&token="
                        , t
                        , "&pl="
                        , pl
                        , "&n="
                        , TE.decodeUtf8 $ urlEncode (TE.encodeUtf8 n)
                        , " .\n\nThanks! - The WeShift Team"
                        ]

mailRequestOff shift reqid name place tokenemails = do
  server  <- liftM (TE.decodeUtf8 . rqServerName) getRequest
  portNum <- liftM rqServerPort getRequest
  liftIO $ mapM_ (\(m,e) -> postmark
                                (B8.unpack postmarkToken)
                                "messages@weshift.org"
                                (T.concat ["Could you cover a shift for ",name," on ", wsFormatDay (sStart shift), "?"])
                                "request"
                                m
                                e)
    (concat $ map (\(t,es) -> map ((,) (T.concat (msg server portNum name place t))) es) tokenemails)
 where msg s p n pl t =
                        [ "Could you cover a shift for "
                        , n
                        , " from "
                        , wsFormatTime (sStart shift)
                        , " to "
                        , wsFormatTime (sStop shift)
                        , " on "
                        , wsFormatDay (sStart shift)
                        , " at "
                        , pName place
                        , "?\n\n"
                        , "If so, click "
                        , "http://"
                        , s
                        , if p /= 80 then (T.pack $ ':' : (show p)) else ""
                        , "/cover?t="
                        , t
                        , "&r="
                        , reqid
                        --, "&pl="
                        --, pl
                        , " and we'll let them know and put it on your schedule."
                        , "\n\nThanks! - The WeShift Team"
                        ]

mailShiftCovered coverer shift emails = do
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  liftIO $ mapM_ (postmark
                     (B8.unpack postmarkToken)
                     "messages@weshift.org"
                     (T.concat [uName coverer," can cover your shift on ", wsFormatDay (sStart shift), "!"])
                     "cover"
                     (T.concat (msg server portNum)))
                emails
 where msg s p =
                        [ uName coverer
                        , " can cover your shift from "
                        , wsFormatTime (sStart shift)
                        , " to "
                        , wsFormatTime (sStop shift)
                        , " on "
                        , wsFormatDay (sStart shift)
                        , "!\n\n"
                        , "It might be good to get in touch with them, just to be safe. We've already switched the shift from your schedule to theirs, so it should show up on your WeShift generated timesheets correctly."
                        , "\n\nThanks! - The WeShift Team"
                        ]


mailOverdueDeadline name email desc time = do
  liftIO $ postmark (B8.unpack postmarkToken) "messages@weshift.org" (T.concat ["Weshift: You missed a deadline, ",name,"."]) "deadline" (T.concat $ msg name desc time) email
    where msg n d t = [ "Hi "
                      , n
                      ,",\n\n"
                      , "You had a deadline due at "
                      , t
                      , " on WeShift."
                      , " It had a description '"
                      , d
                      , "'.\n\n"
                      ,"If you've done it, please mark it as done so it will show up on your timesheet."
                      , "\n\nThanks! - The WeShift Team"
                      ]

mailClaim name otherName email amount desc time = do
  liftIO $ postmark (B8.unpack postmarkToken) "messages@weshift.org" (T.concat ["Weshift: Claim on your shift, ",name,"."]) "deadline"
          (T.concat $ msg name otherName amount desc time) email
    where msg n o a d t = [ "Hi "
                      , n
                      ,",\n\n"
                      , o
                      , " said that they did part of the work for your shift on "
                      , t
                      , " and has asked for "
                      , a
                      , " units. They gave '"
                      , d
                      , "' as a reason.\n\nTo accept this, go to the shift on the monthly calendar. If you disagree, talk to them or a facilitator to cancel the claim."
                      , "\n\nThanks! - The WeShift Team"
                      ]



resetPassword email token = undefined
