{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutOnline where

import Rattletrap.Type.Common
import Rattletrap.Type.ProductAttribute
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Show)

$(deriveJson ''LoadoutOnlineAttribute)

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBits.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let attributes = loadoutAttributeValue loadoutAttribute
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttributes attributes

decodeLoadoutOnlineAttributeBits
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> BitGet LoadoutOnlineAttribute
decodeLoadoutOnlineAttributeBits version objectMap = do
  size <- decodeWord8leBits
  LoadoutOnlineAttribute <$> Monad.replicateM
    (fromIntegral (word8leValue size))
    (decodeProductAttributesBits version objectMap)
