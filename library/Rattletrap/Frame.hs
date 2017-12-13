module Rattletrap.Frame where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.Primitive
import Rattletrap.Replication

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Frame = Frame
  { frameTime :: Float32
  -- ^ Time in seconds since the beginning of the match.
  , frameDelta :: Float32
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , frameReplications :: [Replication]
  } deriving (Eq, Ord, Show)

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

putFrames :: [Frame] -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

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

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
