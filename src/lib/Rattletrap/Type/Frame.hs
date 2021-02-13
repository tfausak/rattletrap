{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Frame where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Type.Replication

import qualified Data.Binary.Bits.Put as BinaryBits

data Frame = Frame
  { frameTime :: Float32le
  -- ^ Time in seconds since the beginning of the match.
  , frameDelta :: Float32le
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , frameReplications :: [Replication]
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Frame)

putFrames :: [Frame] -> BinaryBits.BitPut ()
putFrames frames = case frames of
  [] -> pure ()
  [frame] -> putFrame frame
  first : rest -> do
    putFrame first
    putFrames rest

putFrame :: Frame -> BinaryBits.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
