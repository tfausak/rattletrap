module Rattletrap.Decode.ExplosionAttribute
  ( getExplosionAttribute
  ) where

import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getExplosionAttribute :: BinaryBit.BitGet ExplosionAttribute
getExplosionAttribute = do
  False <- BinaryBit.getBool
  actorId <- getInt32Bits
  location <- getVector
  pure (ExplosionAttribute actorId location)
