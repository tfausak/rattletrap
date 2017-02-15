module Rattletrap.ActorMap where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap

type ActorMap = HashMap.HashMap Word Word32

actorMapEmpty :: ActorMap
actorMapEmpty = HashMap.empty

actorMapInsert :: CompressedWord -> Word32 -> ActorMap -> ActorMap
actorMapInsert compressedWord =
  HashMap.insert (compressedWordValue compressedWord)

actorMapLookup :: CompressedWord -> ActorMap -> Maybe Word32
actorMapLookup compressedWord =
  HashMap.lookup (compressedWordValue compressedWord)
