{-# LANGUAGE TemplateHaskell #-}
module Network.API.Mandrill.Senders( VerifyDomainRq(..)
                                   , vdrq_key
                                   , vdrq_domain
                                   , vdrq_mailbox
                                   , VerifyDomainResponse(..)
                                   , vdres_status
                                   , vdres_domain
                                   , vdres_email
                                   , verifyDomain
                                   ) where

import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Aeson.Types              (fieldLabelModifier)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (decodeUtf8)
import           Lens.Micro.TH                 (makeLenses)
import           Network.API.Mandrill.HTTP     (toMandrillResponse)
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Types
import           Network.HTTP.Client           (Manager)
import qualified Text.Email.Validate           as TEV

data VerifyDomainRq = VerifyDomainRq
    { _vdrq_key     :: MandrillKey
    , _vdrq_domain  :: Text
    , _vdrq_mailbox :: Text
    }
    deriving Show

makeLenses ''VerifyDomainRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''VerifyDomainRq

data VerifyDomainResponse = VerifyDomainResponse
    { _vdres_status :: Text
    , _vdres_domain :: Text
    , _vdres_email  :: MandrillEmail
    }
    deriving Show

makeLenses ''VerifyDomainResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''VerifyDomainResponse

verifyDomain :: MandrillKey
                -- ^ The API key
                -> TEV.EmailAddress
                -- ^ Email address to use for verification
                -> Maybe Manager
                -> IO (MandrillResponse VerifyDomainResponse)
verifyDomain k email =
  toMandrillResponse VerifyDomain
  (VerifyDomainRq k  (decodeUtf8 $ TEV.domainPart email) (decodeUtf8 $ TEV.localPart email))
