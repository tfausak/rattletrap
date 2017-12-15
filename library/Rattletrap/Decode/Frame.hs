module Rattletrap.Decode.Frame
  ( getFrames
  , getFrame
  ) where

import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.Frame
import Rattletrap.Type.Word32
import Rattletrap.Type.CompressedWord
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Replication

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getFrames
  :: (Int, Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32
  -> BinaryBit.BitGet ([Frame], Map.Map CompressedWord Word32)
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
  -> Map.Map CompressedWord Word32
  -> BinaryBit.BitGet (Frame, Map.Map CompressedWord Word32)
getFrame version maxChannels classAttributeMap actorMap = do
  time <- getFloat32Bits
  delta <- getFloat32Bits
  (replications, newActorMap) <- getReplications
    version
    maxChannels
    classAttributeMap
    actorMap
  pure (Frame time delta replications, newActorMap)
