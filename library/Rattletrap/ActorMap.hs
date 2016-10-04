module Rattletrap.ActorMap where

import Rattletrap.CompressedWord
import Rattletrap.Word32

import qualified Data.Bimap as Bimap

type ActorMap = Bimap.Bimap CompressedWord Word32

makeActorMap :: ActorMap
makeActorMap = Bimap.empty

updateActorMap :: CompressedWord -> Word32 -> ActorMap -> ActorMap
updateActorMap = Bimap.insert
