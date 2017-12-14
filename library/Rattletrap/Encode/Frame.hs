module Rattletrap.Encode.Frame
  ( putFrames
  , putFrame
  ) where

import Rattletrap.Type.Frame
import Rattletrap.Encode.Float32
import Rattletrap.Encode.Replication

import qualified Data.Binary.Bits.Put as BinaryBit

putFrames :: [Frame] -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
