module Rattletrap.Replication.Destroyed where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data DestroyedReplication = DestroyedReplication
  {
  } deriving (Eq, Ord, Show)

getDestroyedReplication :: BinaryBit.BitGet DestroyedReplication
getDestroyedReplication = pure DestroyedReplication

putDestroyedReplication :: DestroyedReplication -> BinaryBit.BitPut ()
putDestroyedReplication _ = pure ()
