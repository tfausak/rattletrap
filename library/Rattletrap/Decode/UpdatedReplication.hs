module Rattletrap.Decode.UpdatedReplication
  ( getUpdatedReplication
  ) where

import Rattletrap.Decode.Attribute
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.UpdatedReplication
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getUpdatedReplication
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> CompressedWord
  -> BinaryBit.BitGet UpdatedReplication
getUpdatedReplication version classAttributeMap actorMap actorId = do
  attributes <- decodeAttributesBits version classAttributeMap actorMap actorId
  pure (UpdatedReplication attributes)
