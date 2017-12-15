module Rattletrap.Decode.LoadoutOnlineAttribute
  ( getLoadoutOnlineAttribute
  ) where

import Rattletrap.Decode.ProductAttribute
import Rattletrap.Decode.Word8le
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getLoadoutOnlineAttribute
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> BinaryBit.BitGet LoadoutOnlineAttribute
getLoadoutOnlineAttribute version objectMap = do
  size <- getWord8Bits
  values <- Monad.replicateM
    (fromIntegral (word8leValue size))
    (getProductAttributes version objectMap)
  pure (LoadoutOnlineAttribute values)
