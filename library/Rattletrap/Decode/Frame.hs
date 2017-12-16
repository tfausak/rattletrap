module Rattletrap.Decode.Frame
  ( getFrames
  , getFrame
  ) where

import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Replication
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Frame
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getFrames
  :: (Int, Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BinaryBit.BitGet
       [Frame]
getFrames version numFrames maxChannels classAttributeMap = if numFrames <= 0
  then pure []
  else do
    frame <- getFrame version maxChannels classAttributeMap
    frames <- getFrames version (numFrames - 1) maxChannels classAttributeMap
    pure (frame : frames)

getFrame
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       BinaryBit.BitGet
       Frame
getFrame version maxChannels classAttributeMap = do
  time <- Trans.lift decodeFloat32leBits
  delta <- Trans.lift decodeFloat32leBits
  replications <- getReplications version maxChannels classAttributeMap
  pure (Frame time delta replications)
