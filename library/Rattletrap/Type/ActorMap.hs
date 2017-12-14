module Rattletrap.Type.ActorMap
  ( ActorMap
  , makeActorMap
  , updateActorMap
  ) where

import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Word32

import qualified Data.Map.Strict as Map

type ActorMap = Map.Map CompressedWord Word32

{-# DEPRECATED makeActorMap "use Data.Map.Strict.empty"#-}
makeActorMap :: ActorMap
makeActorMap = Map.empty

{-# DEPRECATED updateActorMap "use Data.Map.Strict.insert"#-}
updateActorMap :: CompressedWord -> Word32 -> ActorMap -> ActorMap
updateActorMap = Map.insert
