module Rattletrap.Frame where

import Debug.Trace
import Text.Printf
import Control.Monad
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
  :: (Int, Int)
  -> (Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet ([Frame], ActorMap)
getFrames version (frameNum, numFrames) maxChannels classAttributeMap actorMap = do
  traceM (printf "- Frame %d (%d remaining):" frameNum numFrames)
  if numFrames <= 0
    then pure ([], actorMap)
    else do
      (frame, newActorMap) <-
        getFrame version maxChannels classAttributeMap actorMap
      (frames, newerActorMap) <-
        getFrames
          version
          (frameNum + 1, numFrames - 1)
          maxChannels
          classAttributeMap
          newActorMap
      pure (frame : frames, newerActorMap)

putFrames :: [Frame] -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

getFrame
  :: (Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Frame, ActorMap)
getFrame version maxChannels classAttributeMap actorMap = do
  time <- getFloat32Bits
  let t = float32Value time
  traceM (printf "    Time: %f" t)
  when (t < 0.01) (fail "time too small")
  delta <- getFloat32Bits
  let d = float32Value delta
  traceM (printf "    Delta: %f" d)
  when (d /= 0 && d < 0.01) (fail "delta too small")
  (replications, newActorMap) <-
    getReplications version maxChannels classAttributeMap actorMap
  pure (Frame time delta replications, newActorMap)

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
