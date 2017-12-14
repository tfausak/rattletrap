module Rattletrap.Encode.LoadoutOnlineAttribute
  ( putLoadoutOnlineAttribute
  ) where

import Rattletrap.Encode.ProductAttribute
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.Word8
import Rattletrap.Encode.Word8

import qualified Data.Binary.Bits.Put as BinaryBit

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBit.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let attributes = loadoutAttributeValue loadoutAttribute
  putWord8Bits (Word8 (fromIntegral (length attributes)))
  mapM_ putProductAttributes attributes
