module Rattletrap.Type.ProductAttribute
  ( ProductAttribute(..)
  ) where

import Rattletrap.Type.Word32
import Rattletrap.Type.Text
import Rattletrap.Type.CompressedWord

import qualified Data.Word as Word

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32
  , productAttributeObjectName :: Maybe Text
  -- ^ read-only
  , productAttributeValue :: Maybe (Either CompressedWord Word.Word32)
  } deriving (Eq, Ord, Show)
