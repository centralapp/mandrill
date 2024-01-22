{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.API.Mandrill.Messages.Types where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                  as T
import           Data.Time
import           Lens.Micro.TH              (makeLenses)

import           Network.API.Mandrill.Types


--------------------------------------------------------------------------------
data MessagesSendRq = MessagesSendRq
    { _msrq_key     :: MandrillKey
    , _msrq_message :: MandrillMessage
    , _msrq_async   :: Maybe Bool
    , _msrq_ip_pool :: Maybe T.Text
    , _msrq_send_at :: Maybe UTCTime
    }
    deriving Show

makeLenses ''MessagesSendRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MessagesSendRq

--------------------------------------------------------------------------------
data MessagesSendTemplateRq = MessagesSendTemplateRq
    { _mstrq_key              :: MandrillKey
    , _mstrq_template_name    :: MandrillTemplate
    , _mstrq_template_content :: [MandrillTemplateContent]
    , _mstrq_message          :: MandrillMessage
    , _mstrq_async            :: Maybe Bool
    , _mstrq_ip_pool          :: Maybe T.Text
    , _mstrq_send_at          :: Maybe UTCTime
    }
    deriving Show

makeLenses ''MessagesSendTemplateRq
deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''MessagesSendTemplateRq

--------------------------------------------------------------------------------
data MessagesResponse = MessagesResponse
    { _mres_email         :: !T.Text
    -- ^ The email address of the recipient
    , _mres_status        :: MandrillEmailStatus
    -- ^ The sending status of the recipient
    , _mres_reject_reason :: Maybe MandrillRejectReason
    -- ^ The reason for the rejection if the recipient status is "rejected"
    , _mres__id           :: !MandrillResponseId
    -- ^ The message's unique id
    }
    deriving Show

-- | Mandrill response ID: usually a text like: @c2a077b5518a4bfd90d794897e3ab21f@.
-- These IDs can be repeated in the webhooks to indicate the current status of a previously sent message, for example. 
newtype MandrillResponseId = MandrillResponseId { _unMandrillResponseId :: T.Text }
                           deriving (Eq, Show, ToJSON, FromJSON) via T.Text 

makeLenses ''MessagesResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MessagesResponse
