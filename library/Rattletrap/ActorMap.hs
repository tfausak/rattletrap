module Rattletrap.ActorMap where

import Rattletrap.Primitive

import qualified Data.Map.Strict as Map

type ActorMap = Map.Map CompressedWord Word32

{-# DEPRECATED
makeActorMap "use Data.Map.Strict.empty"
 #-}

makeActorMap :: ActorMap
makeActorMap = Map.empty

{-# DEPRECATED
updateActorMap "use Data.Map.Strict.insert"
 #-}

updateActorMap :: CompressedWord -> Word32 -> ActorMap -> ActorMap
updateActorMap = Map.insert
