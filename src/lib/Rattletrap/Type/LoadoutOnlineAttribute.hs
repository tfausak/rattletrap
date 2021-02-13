{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutOnlineAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.ProductAttribute
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Ord, Show)

$(deriveJson ''LoadoutOnlineAttribute)

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBits.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let attributes = loadoutAttributeValue loadoutAttribute
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttributes attributes
