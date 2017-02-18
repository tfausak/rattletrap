module Rattletrap.Frame where

import Rattletrap.Map
import Rattletrap.Primitive
import Rattletrap.Replication

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Vector as Vector

data Frame = Frame
  { frameTime :: Float32
  -- ^ Time in seconds since the beginning of the match.
  , frameDelta :: Float32
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , frameReplications :: [Replication]
  } deriving (Eq, Show)

getFrames
  :: (Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Vector.Vector Frame, ActorMap)
getFrames version numFrames maxChannels classAttributeMap actorMap =
  let go n frames actors =
        if n <= 0
          then pure (Vector.fromList (reverse frames), actors)
          else do
            (frame, newActors) <-
              getFrame version maxChannels classAttributeMap actors
            go (n - 1) (frame : frames) newActors
  in go numFrames [] actorMap

putFrames :: Vector.Vector Frame -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

getFrame
  :: (Int, Int)
  -> Word
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Frame, ActorMap)
getFrame version maxChannels classAttributeMap actorMap = do
  time <- getFloat32Bits
  delta <- getFloat32Bits
  (replications, newActorMap) <-
    getReplications version maxChannels classAttributeMap actorMap
  pure (Frame time delta replications, newActorMap)

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
