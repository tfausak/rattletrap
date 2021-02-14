{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Product where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Attribute.ProductValue as ProductValue
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

import qualified Data.Map as Map

data Product = Product
  { unknown :: Bool
  , objectId :: U32.U32
  , objectName :: Maybe Str.Str
  -- ^ read-only
  , value :: ProductValue.ProductValue
  }
  deriving (Eq, Show)

$(deriveJson ''Product)

putProductAttributes :: List.List Product -> BitPut.BitPut
putProductAttributes attributes = do
  let v = List.toList attributes
  U8.bitPut . U8.fromWord8 . fromIntegral $ length v
  mapM_ bitPut v

bitPut :: Product -> BitPut.BitPut
bitPut attribute = do
  BitPut.bool (unknown attribute)
  U32.bitPut (objectId attribute)
  ProductValue.bitPut $ value attribute

decodeProductAttributesBits
  :: (Int, Int, Int) -> Map U32.U32 Str.Str -> BitGet (List.List Product)
decodeProductAttributesBits version objectMap = do
  size <- U8.bitGet
  List.replicateM (fromIntegral $ U8.toWord8 size) $ bitGet version objectMap

bitGet
  :: (Int, Int, Int) -> Map U32.U32 Str.Str -> BitGet Product
bitGet version objectMap = do
  flag <- getBool
  objectId_ <- U32.bitGet
  let maybeObjectName = Map.lookup objectId_ objectMap
  value_ <- ProductValue.bitGet version objectId_ maybeObjectName
  pure (Product flag objectId_ maybeObjectName value_)
