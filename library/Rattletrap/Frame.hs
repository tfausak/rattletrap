module Rattletrap.Frame where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.Float32
import Rattletrap.Replication

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Frame = Frame
  { frameTime :: Float32
  , frameDelta :: Float32
  , frameReplications :: [Replication]
  } deriving (Eq, Ord, Show)

getFrames
  :: (Int, Int)
  -> Int
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet ([Frame], ActorMap)
getFrames version numFrames classAttributeMap actorMap = do
  maybeFrame <-
    if numFrames > 0
      then getFrame version classAttributeMap actorMap
      else pure Nothing
  case maybeFrame of
    Nothing -> pure ([], actorMap)
    Just (frame, newActorMap) -> do
      (frames, newerActorMap) <-
        getFrames version (numFrames - 1) classAttributeMap newActorMap
      pure (frame : frames, newerActorMap)

putFrames :: [Frame] -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

getFrame
  :: (Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> BinaryBit.BitGet (Maybe (Frame, ActorMap))
getFrame version classAttributeMap actorMap = do
  time <- getFloat32Bits
  delta <- getFloat32Bits
  if time == Float32 0 && delta == Float32 0
    then pure Nothing
    else do
      (replications, newActorMap) <-
        getReplications version classAttributeMap actorMap
      pure
        (Just
           ( Frame
             { frameTime = time
             , frameDelta = delta
             , frameReplications = replications
             }
           , newActorMap))

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
