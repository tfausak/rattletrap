module Rattletrap.Encode.UpdatedReplication
  ( putUpdatedReplication
  ) where

import Rattletrap.Encode.Attribute
import Rattletrap.Type.UpdatedReplication

import qualified Data.Binary.Bits.Put as BinaryBits

putUpdatedReplication :: UpdatedReplication -> BinaryBits.BitPut ()
putUpdatedReplication updatedReplication =
  putAttributes (updatedReplicationAttributes updatedReplication)
