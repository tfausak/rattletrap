{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutOnline where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Product as Product
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

import qualified Data.Map as Map

newtype LoadoutOnline = LoadoutOnline
  { value :: List.List (List.List Product.Product)
  } deriving (Eq, Show)

$(deriveJson ''LoadoutOnline)

bitPut :: LoadoutOnline -> BitPut.BitPut
bitPut loadoutAttribute = do
  let attributes = List.toList $ value loadoutAttribute
  U8.bitPut . U8.fromWord8 . fromIntegral $ length attributes
  mapM_ Product.putProductAttributes attributes

bitGet
  :: (Int, Int, Int)
  -> Map.Map U32.U32 Str.Str
  -> BitGet.BitGet LoadoutOnline
bitGet version objectMap = do
  size <- U8.bitGet
  LoadoutOnline <$> List.replicateM
    (fromIntegral (U8.toWord8 size))
    (Product.decodeProductAttributesBits version objectMap)
