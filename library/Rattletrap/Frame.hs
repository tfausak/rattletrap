module Rattletrap.Frame where

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

getFrames :: ClassAttributeMap -> BinaryBit.BitGet [Frame]
getFrames classAttributeMap = do
  maybeFrame <- getFrame classAttributeMap
  case maybeFrame of
    Nothing -> pure []
    Just frame -> do
      frames <- getFrames classAttributeMap
      pure (frame : frames)

putFrames :: [Frame] -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

getFrame :: ClassAttributeMap -> BinaryBit.BitGet (Maybe Frame)
getFrame classAttributeMap = do
  isEmpty <- BinaryBit.isEmpty
  if isEmpty
    then pure Nothing
    else do
      time <- getFloat32Bits
      delta <- getFloat32Bits
      if time == Float32 0 && delta == Float32 0
        then pure Nothing
        else do
          replications <- getReplications classAttributeMap
          pure
            (Just
               Frame
               { frameTime = time
               , frameDelta = delta
               , frameReplications = replications
               })

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
