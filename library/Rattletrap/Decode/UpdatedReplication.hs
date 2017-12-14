module Rattletrap.Decode.UpdatedReplication
  ( getUpdatedReplication
  ) where

import Rattletrap.Type.ActorMap
import Rattletrap.Decode.Attribute
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.UpdatedReplication

import qualified Data.Binary.Bits.Get as BinaryBit

getUpdatedReplication
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet UpdatedReplication
getUpdatedReplication version classAttributeMap actorMap actorId = do
  attributes <- getAttributes version classAttributeMap actorMap actorId
  pure (UpdatedReplication attributes)
