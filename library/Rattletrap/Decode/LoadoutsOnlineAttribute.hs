module Rattletrap.Decode.LoadoutsOnlineAttribute
  ( getLoadoutsOnlineAttribute
  ) where

import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Type.Text
import Rattletrap.Type.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getLoadoutsOnlineAttribute
  :: (Int, Int, Int)
  -> Map.Map Word32 Text
  -> BinaryBit.BitGet LoadoutsOnlineAttribute
getLoadoutsOnlineAttribute version objectMap = do
  blueLoadout <- getLoadoutOnlineAttribute version objectMap
  orangeLoadout <- getLoadoutOnlineAttribute version objectMap
  unknown1 <- BinaryBit.getBool
  unknown2 <- BinaryBit.getBool
  pure (LoadoutsOnlineAttribute blueLoadout orangeLoadout unknown1 unknown2)
