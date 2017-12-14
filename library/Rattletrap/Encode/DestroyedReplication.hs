module Rattletrap.Encode.DestroyedReplication
  ( putDestroyedReplication
  ) where

import Rattletrap.Type.DestroyedReplication

import qualified Data.Binary.Bits.Put as BinaryBit

putDestroyedReplication :: DestroyedReplication -> BinaryBit.BitPut ()
putDestroyedReplication _ = pure ()
