{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute
  ( ProductAttribute(..)
  , ProductAttributeValue(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data ProductAttributeValue
  = ProductAttributeValuePaintedOld CompressedWord
  | ProductAttributeValuePaintedNew Word32
  | ProductAttributeValueUserColorOld (Maybe Word32)
  | ProductAttributeValueUserColorNew Word32le
  | ProductAttributeValueTitleId Str
  deriving (Eq, Ord, Show)

$(deriveJson ''ProductAttributeValue)

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32le
  , productAttributeObjectName :: Maybe Str
  -- ^ read-only
  , productAttributeValue :: Maybe ProductAttributeValue
  } deriving (Eq, Ord, Show)

$(deriveJson ''ProductAttribute)
