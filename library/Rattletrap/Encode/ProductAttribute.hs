module Rattletrap.Encode.ProductAttribute
  ( putProductAttributes
  )
where

import Rattletrap.Encode.Common
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
