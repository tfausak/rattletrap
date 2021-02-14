{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.ProductAttributeValue as ProductAttributeValue
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttribute = ProductAttribute
  { unknown :: Bool
  , objectId :: U32.U32
  , objectName :: Maybe Str.Str
  -- ^ read-only
  , value :: ProductAttributeValue.ProductAttributeValue
  }
  deriving (Eq, Show)

$(deriveJson ''ProductAttribute)

putProductAttributes :: [ProductAttribute] -> BitPut ()
putProductAttributes attributes = do
  U8.bitPut . U8.fromWord8 . fromIntegral $ length attributes
  mapM_ bitPut attributes

bitPut :: ProductAttribute -> BitPut ()
bitPut attribute = do
  BinaryBits.putBool (unknown attribute)
  U32.bitPut (objectId attribute)
  ProductAttributeValue.bitPut $ value attribute

decodeProductAttributesBits
  :: (Int, Int, Int) -> Map U32.U32 Str.Str -> BitGet [ProductAttribute]
decodeProductAttributesBits version objectMap = do
  size <- U8.bitGet
  Monad.replicateM
    (fromIntegral $ U8.toWord8 size)
    (bitGet version objectMap)

bitGet
  :: (Int, Int, Int) -> Map U32.U32 Str.Str -> BitGet ProductAttribute
bitGet version objectMap = do
  flag <- getBool
  objectId_ <- U32.bitGet
  let maybeObjectName = Map.lookup objectId_ objectMap
  value_ <- ProductAttributeValue.bitGet version objectId_ maybeObjectName
  pure (ProductAttribute flag objectId_ maybeObjectName value_)
