{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute
  ( ProductAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32le
  , productAttributeObjectName :: Maybe Str
  -- ^ read-only
  , productAttributeValue :: Maybe (Either CompressedWord Word32)
  } deriving (Eq, Ord, Show)

$(deriveJson ''ProductAttribute)
