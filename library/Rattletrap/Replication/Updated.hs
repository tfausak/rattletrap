module Rattletrap.Replication.Updated where

import Rattletrap.ActorMap
import Rattletrap.Attribute
import Rattletrap.ClassAttributeMap
import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Ord, Show)

getUpdatedReplication
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet UpdatedReplication
getUpdatedReplication version classAttributeMap actorMap actorId = do
  attributes <- getAttributes version classAttributeMap actorMap actorId
  pure (UpdatedReplication attributes)

putUpdatedReplication :: UpdatedReplication -> BinaryBit.BitPut ()
putUpdatedReplication updatedReplication =
  putAttributes (updatedReplicationAttributes updatedReplication)
