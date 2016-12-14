module Rattletrap.ActorMap where

import Rattletrap.Primitive

import qualified Data.Map.Strict as Map

type ActorMap = Map.Map CompressedWord Word32

makeActorMap :: ActorMap
makeActorMap = Map.empty

updateActorMap :: CompressedWord -> Word32 -> ActorMap -> ActorMap
updateActorMap = Map.insert
