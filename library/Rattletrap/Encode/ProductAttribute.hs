module Rattletrap.Encode.ProductAttribute
  ( putProductAttributes
  , putProductAttribute
  ) where

import Rattletrap.Encode.CompressedWord
import Rattletrap.Encode.Word32le
import Rattletrap.Encode.Word8le
import Rattletrap.Encode.Str
import Rattletrap.Type.ProductAttribute
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

putProductAttributes :: [ProductAttribute] -> BinaryBits.BitPut ()
putProductAttributes attributes = do
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BinaryBits.BitPut ()
putProductAttribute attribute = do
  BinaryBits.putBool (productAttributeUnknown attribute)
  putWord32Bits (productAttributeObjectId attribute)
  case productAttributeValue attribute of
    Nothing -> pure ()
    Just (ProductAttributeValuePaintedOld x) -> putCompressedWord x
    Just (ProductAttributeValuePaintedNew x) -> BinaryBits.putWord32be 31 x
    Just (ProductAttributeValueUserColor x) -> case x of
      Nothing -> BinaryBits.putBool False
      Just y -> do
        BinaryBits.putBool True
        BinaryBits.putWord32be 31 y
    Just (ProductAttributeValueTitleId x) -> putTextBits x
