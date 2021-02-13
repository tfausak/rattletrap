{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UpdatedReplication where

import Rattletrap.Type.Attribute
import Rattletrap.Type.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Ord, Show)

$(deriveJson ''UpdatedReplication)

putUpdatedReplication :: UpdatedReplication -> BinaryBits.BitPut ()
putUpdatedReplication updatedReplication =
  putAttributes (updatedReplicationAttributes updatedReplication)
