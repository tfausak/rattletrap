{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Updated where

import Rattletrap.Type.Attribute
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Word32le
import Rattletrap.Encode.Common

import qualified Data.Map as Map

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Show)

$(deriveJson ''UpdatedReplication)

putUpdatedReplication :: UpdatedReplication -> BitPut ()
putUpdatedReplication updatedReplication =
  putAttributes (updatedReplicationAttributes updatedReplication)

decodeUpdatedReplicationBits
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> CompressedWord
  -> BitGet UpdatedReplication
decodeUpdatedReplicationBits version classes actors actor =
  UpdatedReplication <$> decodeAttributesBits version classes actors actor
