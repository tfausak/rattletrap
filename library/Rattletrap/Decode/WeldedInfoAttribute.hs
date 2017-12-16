module Rattletrap.Decode.WeldedInfoAttribute
  ( getWeldedInfoAttribute
  ) where

import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Int8Vector
import Rattletrap.Decode.Vector
import Rattletrap.Type.WeldedInfoAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getWeldedInfoAttribute :: BinaryBit.BitGet WeldedInfoAttribute
getWeldedInfoAttribute = do
  active <- BinaryBit.getBool
  actorId <- getInt32Bits
  offset <- getVector
  mass <- decodeFloat32leBits
  rotation <- getInt8Vector
  pure (WeldedInfoAttribute active actorId offset mass rotation)
