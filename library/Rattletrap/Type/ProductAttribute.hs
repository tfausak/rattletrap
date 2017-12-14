{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ProductAttribute
  ( ProductAttribute(..)
  ) where

import Rattletrap.Type.Common
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
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON ProductAttribute where
  parseJSON = defaultParseJson "ProductAttribute"

instance ToJSON ProductAttribute where
  toEncoding = defaultToEncoding "ProductAttribute"
  toJSON = defaultToJson "ProductAttribute"
