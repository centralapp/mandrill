module Network.API.Mandrill.Templates
  ( getTemplates
  -- * Re-exports.
  , module Network.API.Mandrill.Templates.Types
  ) where

import           Network.API.Mandrill.HTTP
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Templates.Types
import           Network.API.Mandrill.Types
import           Network.HTTP.Client

getTemplates :: MandrillKey -> Maybe Label -> Maybe Manager -> IO (MandrillResponse [TemplatesResponse])
getTemplates k labelFilt = toMandrillResponse GetTemplates (GetTemplatesRq k labelFilt)
