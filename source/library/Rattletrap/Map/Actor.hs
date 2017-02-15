module Rattletrap.Map.Actor where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap

newtype ActorMap = ActorMap
  { actorMapValue :: HashMap.HashMap Word Word32
  } deriving (Eq, Show)

actorMapEmpty :: ActorMap
actorMapEmpty = ActorMap HashMap.empty

actorMapInsert :: CompressedWord -> Word32 -> ActorMap -> ActorMap
actorMapInsert compressedWord objectId (ActorMap actorMap) =
  ActorMap
    (HashMap.insert (compressedWordValue compressedWord) objectId actorMap)

actorMapLookup :: CompressedWord -> ActorMap -> Maybe Word32
actorMapLookup compressedWord (ActorMap actorMap) =
  HashMap.lookup (compressedWordValue compressedWord) actorMap
