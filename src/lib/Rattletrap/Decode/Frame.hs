module Rattletrap.Decode.Frame
  ( decodeFramesBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Replication
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Frame
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

decodeFramesBits
  :: (Int, Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord Word32le)
       DecodeBits
       [Frame]
decodeFramesBits version count limit classes = if count <= 0
  then pure []
  else
    (:)
    <$> decodeFrameBits version limit classes
    <*> decodeFramesBits version (count - 1) limit classes

decodeFrameBits
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap
  -> State.StateT (Map.Map CompressedWord Word32le) DecodeBits Frame
decodeFrameBits version limit classes =
  Frame
    <$> Trans.lift decodeFloat32leBits
    <*> Trans.lift decodeFloat32leBits
    <*> decodeReplicationsBits version limit classes
