module Rattletrap.Decode.Frame
  ( getFrames
  , getFrame
  ) where

import Rattletrap.Type.ActorMap
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.Frame
import Rattletrap.Decode.Float32
import Rattletrap.Decode.Replication

import qualified Data.Binary.Bits.Get as BinaryBit

getFrames
  :: (Int, Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet ([Frame], ActorMap)
getFrames version numFrames maxChannels classAttributeMap actorMap =
  if numFrames <= 0
    then pure ([], actorMap)
    else do
      (frame, newActorMap) <- getFrame
        version
        maxChannels
        classAttributeMap
        actorMap
      (frames, newerActorMap) <- getFrames
        version
        (numFrames - 1)
        maxChannels
        classAttributeMap
        newActorMap
      pure (frame : frames, newerActorMap)

getFrame
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Frame, ActorMap)
getFrame version maxChannels classAttributeMap actorMap = do
  time <- getFloat32Bits
  delta <- getFloat32Bits
  (replications, newActorMap) <- getReplications
    version
    maxChannels
    classAttributeMap
    actorMap
  pure (Frame time delta replications, newActorMap)
