module Rattletrap.Decode.WeldedInfoAttribute
  ( getWeldedInfoAttribute
  ) where

import Rattletrap.Type.WeldedInfoAttribute
import Rattletrap.Decode.Int32
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Int8Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getWeldedInfoAttribute :: BinaryBit.BitGet WeldedInfoAttribute
getWeldedInfoAttribute = do
  active <- BinaryBit.getBool
  actorId <- getInt32Bits
  offset <- getVector
  mass <- getFloat32Bits
  rotation <- getInt8Vector
  pure (WeldedInfoAttribute active actorId offset mass rotation)
