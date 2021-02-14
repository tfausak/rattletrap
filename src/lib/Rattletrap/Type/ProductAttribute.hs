{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import qualified Rattletrap.Type.ProductAttributeValue as ProductAttributeValue
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttribute = ProductAttribute
  { unknown :: Bool
  , objectId :: Word32le.Word32le
  , objectName :: Maybe Str.Str
  -- ^ read-only
  , value :: ProductAttributeValue.ProductAttributeValue
  }
  deriving (Eq, Show)

$(deriveJson ''ProductAttribute)

putProductAttributes :: [ProductAttribute] -> BitPut ()
putProductAttributes attributes = do
  Word8le.bitPut . Word8le.fromWord8 . fromIntegral $ length attributes
  mapM_ bitPut attributes

bitPut :: ProductAttribute -> BitPut ()
bitPut attribute = do
  BinaryBits.putBool (unknown attribute)
  Word32le.bitPut (objectId attribute)
  ProductAttributeValue.bitPut $ value attribute

decodeProductAttributesBits
  :: (Int, Int, Int) -> Map Word32le.Word32le Str.Str -> BitGet [ProductAttribute]
decodeProductAttributesBits version objectMap = do
  size <- Word8le.bitGet
  Monad.replicateM
    (fromIntegral $ Word8le.toWord8 size)
    (bitGet version objectMap)

bitGet
  :: (Int, Int, Int) -> Map Word32le.Word32le Str.Str -> BitGet ProductAttribute
bitGet version objectMap = do
  flag <- getBool
  objectId_ <- Word32le.bitGet
  let maybeObjectName = Map.lookup objectId_ objectMap
  value_ <- ProductAttributeValue.bitGet version objectId_ maybeObjectName
  pure (ProductAttribute flag objectId_ maybeObjectName value_)
