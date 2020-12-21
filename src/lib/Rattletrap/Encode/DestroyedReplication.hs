module Rattletrap.Encode.DestroyedReplication
  ( putDestroyedReplication
  )
where

import Rattletrap.Type.DestroyedReplication

import qualified Data.Binary.Bits.Put as BinaryBits

putDestroyedReplication :: DestroyedReplication -> BinaryBits.BitPut ()
putDestroyedReplication _ = pure ()
