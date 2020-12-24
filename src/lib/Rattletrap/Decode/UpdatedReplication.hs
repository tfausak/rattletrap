module Rattletrap.Decode.UpdatedReplication
  ( decodeUpdatedReplicationBits
  )
where

import Rattletrap.Decode.Attribute
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.UpdatedReplication
import Rattletrap.Type.Word32le

import qualified Data.Map as Map

decodeUpdatedReplicationBits
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> CompressedWord
  -> DecodeBits UpdatedReplication
decodeUpdatedReplicationBits version classes actors actor =
  UpdatedReplication <$> decodeAttributesBits version classes actors actor
