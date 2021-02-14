{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutOnline where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.ProductAttribute as ProductAttribute
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { value :: [[ProductAttribute.ProductAttribute]]
  } deriving (Eq, Show)

$(deriveJson ''LoadoutOnlineAttribute)

bitPut :: LoadoutOnlineAttribute -> BitPut ()
bitPut loadoutAttribute = do
  let attributes = value loadoutAttribute
  U8.bitPut (U8.fromWord8 (fromIntegral (length attributes)))
  mapM_ ProductAttribute.putProductAttributes attributes

bitGet
  :: (Int, Int, Int)
  -> Map.Map U32.U32 Str.Str
  -> BitGet LoadoutOnlineAttribute
bitGet version objectMap = do
  size <- U8.bitGet
  LoadoutOnlineAttribute <$> Monad.replicateM
    (fromIntegral (U8.toWord8 size))
    (ProductAttribute.decodeProductAttributesBits version objectMap)
