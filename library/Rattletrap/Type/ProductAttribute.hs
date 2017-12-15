{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute
  ( ProductAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Str
import Rattletrap.Type.CompressedWord

import qualified Data.Word as Word

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32le
  , productAttributeObjectName :: Maybe Str
  -- ^ read-only
  , productAttributeValue :: Maybe (Either CompressedWord Word.Word32)
  } deriving (Eq, Ord, Show)

$(deriveJson ''ProductAttribute)
