{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.API.Mandrill.Webhooks( EventHook(..)
                                    , WebhookAddRq(..)
                                    , warq_key
                                    , warq_url
                                    , warq_description
                                    , warq_events
                                    ) where

import           Control.Applicative        (pure)
import           Data.Aeson                 (FromJSON, ToJSON, Value (String),
                                             parseJSON, toJSON)
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import           Data.Aeson.Types           (fieldLabelModifier, typeMismatch)
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lens.Micro.TH              (makeLenses)
import           Network.API.Mandrill.Types

data EventHook = EventSent
    | EventDeferred
    | EventHardBounced
    | EventSoftBounced
    | EventOpened
    | EventClicked
    | EventMarkedAsSpam
    | EventUnsubscribed
    | EventRejected
    deriving (Ord, Eq)

instance Show EventHook where
  show e = case e of
    EventSent         -> "send"
    EventDeferred     -> "deferral"
    EventSoftBounced  -> "soft_bounce"
    EventHardBounced  -> "hard_bounce"
    EventOpened       -> "open"
    EventClicked      -> "click"
    EventMarkedAsSpam -> "spam"
    EventUnsubscribed -> "unsub"
    EventRejected     -> "reject"
instance FromJSON EventHook where
  parseJSON (String s) =
    let sanitised = T.toLower s
        err = fail $ "Unable to parse mandrill event: " ++ T.unpack sanitised ++ ", expected one of: " ++ show mandrillEvts
    in maybe err pure . lookup sanitised $ mandrillEvts
  parseJSON val = typeMismatch "String" val

mandrillEvts :: [(T.Text, EventHook)]
mandrillEvts =
    [  ("send"        , EventSent)
    , ("deferral"    , EventDeferred)
    , ("soft_bounce" , EventSoftBounced)
    , ("hard_bounce" , EventHardBounced)
    , ("open"        , EventOpened)
    , ("click"       , EventClicked)
    , ("spam"        , EventMarkedAsSpam)
    , ("reject"      , EventRejected)
    , ("unsub"       , EventUnsubscribed)
    ]

instance ToJSON EventHook where
  toJSON = String . T.pack . show

data WebhookAddRq = WebhookAddRq
    { _warq_key         :: MandrillKey
    , _warq_url         :: Text
    , _warq_description :: Text
    , _warq_events      :: Set EventHook
    }
    deriving Show

makeLenses ''WebhookAddRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''WebhookAddRq
