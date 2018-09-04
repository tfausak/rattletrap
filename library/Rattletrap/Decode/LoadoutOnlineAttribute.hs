module Rattletrap.Decode.LoadoutOnlineAttribute
  ( decodeLoadoutOnlineAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.ProductAttribute
import Rattletrap.Decode.Word8le
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

import qualified Control.Monad as Monad
import qualified Data.Map as Map

decodeLoadoutOnlineAttributeBits
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> DecodeBits LoadoutOnlineAttribute
decodeLoadoutOnlineAttributeBits version objectMap = do
  size <- decodeWord8leBits
  LoadoutOnlineAttribute <$> Monad.replicateM
    (fromIntegral (word8leValue size))
    (decodeProductAttributesBits version objectMap)
