module Rattletrap.Encode.Frame
  ( putFrames
  , putFrame
  )
where

import Rattletrap.Encode.Float32le
import Rattletrap.Encode.Replication
import Rattletrap.Type.Frame

import qualified Data.Binary.Bits.Put as BinaryBits

putFrames :: [Frame] -> BinaryBits.BitPut ()
putFrames = mapM_ putFrame

putFrame :: Frame -> BinaryBits.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
