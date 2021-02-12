{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Encode.Common
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttributeValue
  = ProductAttributeValuePaintedOld CompressedWord
  | ProductAttributeValuePaintedNew Word32
  | ProductAttributeValueTeamEditionOld CompressedWord
  | ProductAttributeValueTeamEditionNew Word32
  | ProductAttributeValueSpecialEdition Word32
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
  , productAttributeValue :: ProductAttributeValue
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''ProductAttribute)

putProductAttributes :: [ProductAttribute] -> BinaryBits.BitPut ()
putProductAttributes attributes = do
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BinaryBits.BitPut ()
putProductAttribute attribute = do
  BinaryBits.putBool (productAttributeUnknown attribute)
  putWord32Bits (productAttributeObjectId attribute)
  case productAttributeValue attribute of
    ProductAttributeValuePaintedOld x -> putCompressedWord x
    ProductAttributeValuePaintedNew x -> putBitsLE 31 x
    ProductAttributeValueTeamEditionOld x -> putCompressedWord x
    ProductAttributeValueTeamEditionNew x -> putBitsLE 31 x
    ProductAttributeValueSpecialEdition x -> putBitsLE 31 x
    ProductAttributeValueUserColorOld x -> case x of
      Nothing -> BinaryBits.putBool False
      Just y -> do
        BinaryBits.putBool True
        putBitsLE 31 y
    ProductAttributeValueUserColorNew x -> putWord32Bits x
    ProductAttributeValueTitleId x -> putTextBits x
