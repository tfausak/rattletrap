module Rattletrap.Decode.DestroyedReplication
  ( getDestroyedReplication
  ) where

import Rattletrap.Type.DestroyedReplication

import qualified Data.Binary.Bits.Get as BinaryBit

getDestroyedReplication :: BinaryBit.BitGet DestroyedReplication
getDestroyedReplication = pure DestroyedReplication
