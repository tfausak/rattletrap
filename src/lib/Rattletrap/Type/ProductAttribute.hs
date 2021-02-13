{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le
import Rattletrap.Type.ProductAttributeValue
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32le
  , productAttributeObjectName :: Maybe Str
  -- ^ read-only
  , productAttributeValue :: ProductAttributeValue
  }
  deriving (Eq, Show)

$(deriveJson ''ProductAttribute)

putProductAttributes :: [ProductAttribute] -> BitPut ()
putProductAttributes attributes = do
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BitPut ()
putProductAttribute attribute = do
  BinaryBits.putBool (productAttributeUnknown attribute)
  putWord32Bits (productAttributeObjectId attribute)
  putProductAttributeValue $ productAttributeValue attribute

decodeProductAttributesBits
  :: (Int, Int, Int) -> Map Word32le Str -> BitGet [ProductAttribute]
decodeProductAttributesBits version objectMap = do
  size <- decodeWord8leBits
  Monad.replicateM
    (fromIntegral (word8leValue size))
    (decodeProductAttributeBits version objectMap)

decodeProductAttributeBits
  :: (Int, Int, Int) -> Map Word32le Str -> BitGet ProductAttribute
decodeProductAttributeBits version objectMap = do
  flag <- getBool
  objectId <- decodeWord32leBits
  let maybeObjectName = Map.lookup objectId objectMap
  value <- decodeProductAttributeValueBits version objectId maybeObjectName
  pure (ProductAttribute flag objectId maybeObjectName value)
