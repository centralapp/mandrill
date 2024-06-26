{-| This package is an attempt to expose the Mandrill JSON API in pure Haskell.
    To do that, the library API comes in two flavours:

    * An IO-based, low-level 1:1 mapping of the JSON API,
      as described on <https://mandrillapp.com/api/docs/ the website>.
    * A handy monad transformer which can be plugged in your stack of choice.
-}

module Network.API.Mandrill (
    module M
  , sendEmail
  , sendTextEmail
  , emptyMessage
  , newTextMessage
  , newHtmlMessage
  , newTemplateMessage
  , newTemplateMessage'
  , liftIO

  -- * Appendix: Example Usage
  -- $exampleusage
  ) where

import           Control.Monad.Reader
import qualified Data.Text                           as T
import           Data.Time
import           Network.API.Mandrill.Messages       as M
import           Network.API.Mandrill.Messages.Types as M
import           Network.API.Mandrill.Trans          as M
import           Network.API.Mandrill.Types          as M
import           Text.Blaze.Html
import           Text.Email.Validate

{- $exampleusage

The API was designed to allow to get you started as quickly as possible:

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Email.Validate
> import Network.API.Mandrill
>
> main :: IO ()
> main = do
>   case validate "foo@example.com" of
>     Left err   -> print $ "Invalid email!" ++ show err
>     Right addr -> runMandrill "MYTOKENHERE" $ do
>       let msg = "<p>My Html</p>"
>       res <- sendEmail (newTextMessage addr [addr] "Hello" msg)
>       case res of
>         MandrillSuccess k -> liftIO (print k)
>         MandrillFailure f -> liftIO (print f)

-}

--------------------------------------------------------------------------------
-- | Builds an empty message, given only the email of the sender and
-- the emails of the receiver. Please note that the "Subject" will be empty,
-- so you need to use either @newTextMessage@ or @newHtmlMessage@ to populate it.
emptyMessage :: Maybe EmailAddress -> [EmailAddress] -> MandrillMessage
emptyMessage f t = MandrillMessage {
   _mmsg_html = mempty
 , _mmsg_text = Nothing
 , _mmsg_subject = Nothing
 , _mmsg_from_email = MandrillEmail <$> f
 , _mmsg_from_name = Nothing
 , _mmsg_to = map newRecipient t
 , _mmsg_headers = mempty
 , _mmsg_important = Nothing
 , _mmsg_track_opens = Nothing
 , _mmsg_track_clicks = Nothing
 , _mmsg_auto_text = Nothing
 , _mmsg_auto_html = Nothing
 , _mmsg_inline_css = Nothing
 , _mmsg_url_strip_qs = Nothing
 , _mmsg_preserve_recipients = Nothing
 , _mmsg_view_content_link = Nothing
 , _mmsg_bcc_address = Nothing
 , _mmsg_tracking_domain = Nothing
 , _mmsg_signing_domain = Nothing
 , _mmsg_return_path_domain = Nothing
 , _mmsg_merge = Nothing
 , _mmsg_global_merge_vars = []
 , _mmsg_merge_vars = []
 , _mmsg_tags = []
 , _mmsg_subaccount = Nothing
 , _mmsg_google_analytics_domains = []
 , _mmsg_google_analytics_campaign = Nothing
 , _mmsg_metadata = mempty
 , _mmsg_recipient_metadata = []
 , _mmsg_attachments = []
 , _mmsg_images = []
  }


--------------------------------------------------------------------------------
-- | Create a new HTML message.
newHtmlMessage :: EmailAddress
               -- ^ Sender email
               -> [EmailAddress]
               -- ^ Receivers email
               -> T.Text
               -- ^ Subject
               -> Html
               -- ^ The HTML body
               -> MandrillMessage
newHtmlMessage f t subj html = let body = mkMandrillHtml html in
  (emptyMessage (Just f) t) { _mmsg_html = body, _mmsg_subject = Just subj }

--------------------------------------------------------------------------------
-- | Create a new template message (no HTML).
newTemplateMessage :: EmailAddress
                   -- ^ Sender email
                   -> [EmailAddress]
                   -- ^ Receivers email
                   -> T.Text
                   -- ^ Subject
                   -> MandrillMessage
newTemplateMessage f t subj = (emptyMessage (Just f) t) { _mmsg_subject = Just subj }

--------------------------------------------------------------------------------
-- | Create a new template message (no HTML) with recipient addresses only.
-- This function is preferred when the template being used has the sender
-- address and subject already configured in the Mandrill server.
newTemplateMessage' :: [EmailAddress]
                    -- ^ Receivers email
                    -> MandrillMessage
newTemplateMessage' = emptyMessage Nothing

--------------------------------------------------------------------------------
-- | Create a new textual message. By default Mandrill doesn't require you
-- to specify the @mmsg_text@ when sending out the JSON Payload, and this
-- function ensure it will be present.
newTextMessage :: EmailAddress
               -- ^ Sender email
               -> [EmailAddress]
               -- ^ Receivers email
               -> T.Text
               -- ^ Subject
               -> T.Text
               -- ^ The body, as normal text.
               -> MandrillMessage
newTextMessage f t subj txt = let body = unsafeMkMandrillHtml txt in
  (emptyMessage (Just f) t) {
       _mmsg_html = body
     , _mmsg_text = Just txt
     , _mmsg_subject = Just subj
     }


--------------------------------------------------------------------------------
-- | The simplest way to use the API. All you need to provide is a valid
-- 'MandrillMessage' and this function will send an email inside a
-- 'MandrillT' transformer. You are not forced to use the 'MandrillT' context
-- though. Have a look at "Network.API.Mandrill.Messages" for an IO-based,
-- low level function for sending email.
sendEmail :: MonadIO m
          => MandrillMessage
          -> MandrillT m (MandrillResponse [MessagesResponse])
sendEmail msg = do
  (key, mgr) <- ask
  liftIO $ send key msg (Just True) Nothing Nothing (Just mgr)


--------------------------------------------------------------------------------
sendTextEmail :: MonadIO m
              => MandrillMessage
              -> MandrillT m (MandrillResponse [MessagesResponse])
sendTextEmail msg = do
  (key, mgr) <- ask
  now <- liftIO getCurrentTime
  liftIO $ send key msg (Just True) Nothing (Just now) (Just mgr)
