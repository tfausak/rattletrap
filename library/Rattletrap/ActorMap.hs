module Rattletrap.ActorMap where

import Rattletrap.Primitive.CompressedWord
import Rattletrap.Word32

import qualified Data.Map as Map

type ActorMap = Map.Map CompressedWord Word32

makeActorMap :: ActorMap
makeActorMap = Map.empty

updateActorMap :: CompressedWord -> Word32 -> ActorMap -> ActorMap
updateActorMap actorId objectId actorMap = Map.insert actorId objectId actorMap
