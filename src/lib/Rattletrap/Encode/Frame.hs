module Rattletrap.Encode.Frame
  ( putFrames
  ) where

import Rattletrap.Encode.Float32le
import Rattletrap.Type.Replication
import Rattletrap.Type.Frame

import qualified Data.Binary.Bits.Put as BinaryBits

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
