{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UpdatedReplication where

import Rattletrap.Type.Attribute
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Word32le

import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

newtype UpdatedReplication = UpdatedReplication
  { updatedReplicationAttributes :: [Attribute]
  } deriving (Eq, Show)

$(deriveJson ''UpdatedReplication)

putUpdatedReplication :: UpdatedReplication -> BinaryBits.BitPut ()
putUpdatedReplication updatedReplication =
  putAttributes (updatedReplicationAttributes updatedReplication)

decodeUpdatedReplicationBits
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> CompressedWord
  -> DecodeBits UpdatedReplication
decodeUpdatedReplicationBits version classes actors actor =
  UpdatedReplication <$> decodeAttributesBits version classes actors actor
