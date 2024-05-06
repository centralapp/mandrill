
module Network.API.Mandrill.Messages
  ( send
  , sendTemplate
  , sendTemplateMod
  )
where

import qualified Data.Text                     as T
import           Data.Time
import           Network.API.Mandrill.HTTP
import           Network.API.Mandrill.Messages.Types
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Types
import           Network.HTTP.Client

--------------------------------------------------------------------------------
-- | Send a new transactional message through Mandrill
send :: MandrillKey
     -- ^ The API key
     -> MandrillMessage
     -- ^ The email message
     -> Maybe Bool
     -- ^ Enable a background sending mode that is optimized for bulk sending
     -> Maybe T.Text
     -- ^ ip_pool
     -> Maybe UTCTime
     -- ^ send_at
     -> Maybe Manager
     -> IO (MandrillResponse [MessagesResponse])
send k msg async ip_pool send_at = toMandrillResponse MessagesSend (MessagesSendRq k msg async ip_pool send_at)

-- | Send a new transactional message through Mandrill using a template
sendTemplate :: MandrillKey
             -- ^ The API key
             -> MandrillTemplate
             -- ^ The template name
             -> [MandrillTemplateContent]
             -- ^ Template content for 'editable regions'
             -> MandrillMessage
             -- ^ The email message
             -> Maybe Bool
             -- ^ Enable a background sending mode that is optimized for bulk sending
             -> Maybe T.Text
             -- ^ ip_pool
             -> Maybe UTCTime
             -- ^ send_at
             -> Maybe Manager
             -> IO (MandrillResponse [MessagesResponse])
sendTemplate = sendTemplateMod id

{- | Send a new transactional message through Mandrill using a template. The first parameter
allows us to modify the 'Request' before it's sent, for example to add headers.
-}
sendTemplateMod :: (Request -> Request)
                -> MandrillKey
                -- ^ The API key
                -> MandrillTemplate
                -- ^ The template name
                -> [MandrillTemplateContent]
                -- ^ Template content for 'editable regions'
                -> MandrillMessage
                -- ^ The email message
                -> Maybe Bool
                -- ^ Enable a background sending mode that is optimized for bulk sending
                -> Maybe T.Text
                -- ^ ip_pool
                -> Maybe UTCTime
                -- ^ send_at
                -> Maybe Manager
                -> IO (MandrillResponse [MessagesResponse])
sendTemplateMod modR k template content msg async ip_pool send_at =
  toMandrillResponseMod modR MessagesSendTemplate (MessagesSendTemplateRq k template content msg async ip_pool send_at)
