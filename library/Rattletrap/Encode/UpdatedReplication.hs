module Rattletrap.Encode.UpdatedReplication
  ( putUpdatedReplication
  ) where

import Rattletrap.Encode.Attribute
import Rattletrap.Type.UpdatedReplication

import qualified Data.Binary.Bits.Put as BinaryBit

putUpdatedReplication :: UpdatedReplication -> BinaryBit.BitPut ()
putUpdatedReplication updatedReplication =
  putAttributes (updatedReplicationAttributes updatedReplication)
