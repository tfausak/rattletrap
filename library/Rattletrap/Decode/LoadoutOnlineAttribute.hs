module Rattletrap.Decode.LoadoutOnlineAttribute
  ( getLoadoutOnlineAttribute
  ) where

import Rattletrap.Decode.ProductAttribute
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.Word32
import Rattletrap.Type.Text
import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getLoadoutOnlineAttribute
  :: (Int, Int, Int)
  -> Map.Map Word32 Text
  -> BinaryBit.BitGet LoadoutOnlineAttribute
getLoadoutOnlineAttribute version objectMap = do
  size <- getWord8Bits
  values <- Monad.replicateM
    (fromIntegral (word8Value size))
    (getProductAttributes version objectMap)
  pure (LoadoutOnlineAttribute values)
