module Rattletrap.Replication.Destroyed where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data DestroyedReplication = DestroyedReplication
  {
  } deriving (Eq, Ord, Show)

getDestroyedReplication :: BinaryBit.BitGet DestroyedReplication
getDestroyedReplication = pure DestroyedReplication

putDestroyedReplication :: DestroyedReplication -> BinaryBit.BitPut ()
putDestroyedReplication _ = pure ()
