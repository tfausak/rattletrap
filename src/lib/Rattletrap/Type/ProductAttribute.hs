{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Type.ProductAttributeValue
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32le.Word32le
  , productAttributeObjectName :: Maybe Str.Str
  -- ^ read-only
  , productAttributeValue :: ProductAttributeValue
  }
  deriving (Eq, Show)

$(deriveJson ''ProductAttribute)

putProductAttributes :: [ProductAttribute] -> BitPut ()
putProductAttributes attributes = do
  Word8le.bitPut . Word8le.fromWord8 . fromIntegral $ length attributes
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BitPut ()
putProductAttribute attribute = do
  BinaryBits.putBool (productAttributeUnknown attribute)
  Word32le.bitPut (productAttributeObjectId attribute)
  putProductAttributeValue $ productAttributeValue attribute

decodeProductAttributesBits
  :: (Int, Int, Int) -> Map Word32le.Word32le Str.Str -> BitGet [ProductAttribute]
decodeProductAttributesBits version objectMap = do
  size <- Word8le.bitGet
  Monad.replicateM
    (fromIntegral $ Word8le.toWord8 size)
    (decodeProductAttributeBits version objectMap)

decodeProductAttributeBits
  :: (Int, Int, Int) -> Map Word32le.Word32le Str.Str -> BitGet ProductAttribute
decodeProductAttributeBits version objectMap = do
  flag <- getBool
  objectId <- Word32le.bitGet
  let maybeObjectName = Map.lookup objectId objectMap
  value <- decodeProductAttributeValueBits version objectId maybeObjectName
  pure (ProductAttribute flag objectId maybeObjectName value)
