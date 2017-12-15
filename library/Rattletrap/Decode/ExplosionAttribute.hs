module Rattletrap.Decode.ExplosionAttribute
  ( getExplosionAttribute
  ) where

import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Type.ExplosionAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getExplosionAttribute :: BinaryBit.BitGet ExplosionAttribute
getExplosionAttribute = do
  flag <- BinaryBit.getBool
  actorId <- getInt32Bits
  location <- getVector
  pure (ExplosionAttribute flag actorId location)
