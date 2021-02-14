{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Frame where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import qualified Rattletrap.Type.Replication as Replication
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

data Frame = Frame
  { time :: Float32le.Float32le
  -- ^ Time in seconds since the beginning of the match.
  , delta :: Float32le.Float32le
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , replications :: [Replication.Replication]
  }
  deriving (Eq, Show)

$(deriveJson ''Frame)

putFrames :: [Frame] -> BitPut ()
putFrames frames = case frames of
  [] -> pure ()
  [frame] -> bitPut frame
  first : rest -> do
    bitPut first
    putFrames rest

bitPut :: Frame -> BitPut ()
bitPut frame = do
  Float32le.bitPut (time frame)
  Float32le.bitPut (delta frame)
  Replication.putReplications (replications frame)

decodeFramesBits
  :: (Int, Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord Word32le.Word32le)
       BitGet
       [Frame]
decodeFramesBits version count limit classes = if count <= 0
  then pure []
  else
    (:)
    <$> bitGet version limit classes
    <*> decodeFramesBits version (count - 1) limit classes

bitGet
  :: (Int, Int, Int)
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT (Map.Map CompressedWord.CompressedWord Word32le.Word32le) BitGet Frame
bitGet version limit classes =
  Frame
    <$> Trans.lift Float32le.bitGet
    <*> Trans.lift Float32le.bitGet
    <*> Replication.decodeReplicationsBits version limit classes
