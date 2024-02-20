{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.API.Mandrill.Templates.Types
  ( -- * Getting templates.
    GetTemplatesRq(..)
  , gtrq_key
  , gtrq_label
  , Label(..)
  , unLabel
  -- * Template data
  , TemplatesResponse(..)
  , tres_slug
  , tres_name
  , tres_labels
  , Slug(..)
  , unSlug
  , Name(..)
  , unName
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Lens.Micro.TH              (makeLenses)

import           Network.API.Mandrill.Types

-- | Request to get templates
data GetTemplatesRq = GetTemplatesRq
    { _gtrq_key   :: MandrillKey -- ^ API Key
    -- ^ An optional Template label (for searching)
    , _gtrq_label :: Maybe Label -- ^ An optional Template label (for searching)
    }
    deriving Show

-- | Template label.
newtype Label = Label { _unLabel :: T.Text }
              deriving (Eq, Show, ToJSON, FromJSON, IsString) via T.Text

makeLenses ''GetTemplatesRq
makeLenses ''Label

-- Drop the leading @_gtrq_@ in all json keys.
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''GetTemplatesRq

-- * Responses

newtype Slug = Slug { _unSlug :: T.Text }
             deriving (Eq, Show, ToJSON, FromJSON, IsString) via T.Text

newtype Name = Name { _unName :: T.Text }
             deriving (Eq, Show, ToJSON, FromJSON, IsString) via T.Text

-- | For templates responses, we're currently only interested in the name and slug of the template.
-- FIXME: add full support.
data TemplatesResponse = TemplatesResponse
    { _tres_slug   :: Slug
    , _tres_name   :: Name
    , _tres_labels :: [Label]
    }
    deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''TemplatesResponse
makeLenses ''TemplatesResponse
makeLenses ''Slug
makeLenses ''Name
