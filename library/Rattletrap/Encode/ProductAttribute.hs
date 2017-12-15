module Rattletrap.Encode.ProductAttribute
  ( putProductAttributes
  , putProductAttribute
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Type.ProductAttribute
import Rattletrap.Encode.Word32le
import Rattletrap.Type.Str
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.Word8le
import Rattletrap.Encode.Word8le

import qualified Data.Binary.Bits.Put as BinaryBit

putProductAttributes :: [ProductAttribute] -> BinaryBit.BitPut ()
putProductAttributes attributes = do
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BinaryBit.BitPut ()
putProductAttribute attribute = do
  BinaryBit.putBool (productAttributeUnknown attribute)
  putWord32Bits (productAttributeObjectId attribute)
  case productAttributeObjectName attribute of
    Just name -> case fromStr name of
      "TAGame.ProductAttribute_Painted_TA" ->
        case productAttributeValue attribute of
          Nothing -> pure ()
          Just (Left x) -> putCompressedWord x
          Just (Right x) -> BinaryBit.putWord32be 31 x
      "TAGame.ProductAttribute_UserColor_TA" ->
        case productAttributeValue attribute of
          Nothing -> BinaryBit.putBool False
          Just value -> do
            BinaryBit.putBool True
            case value of
              Left x -> putCompressedWord x
              Right x -> BinaryBit.putWord32be 31 x
      _ ->
        fail ("unknown object name for product attribute " <> show attribute)
    Nothing ->
      fail ("missing object name for product attribute " <> show attribute)
