{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Frame where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.Replication as Replication
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

data Frame = Frame
  { time :: F32.F32
  -- ^ Time in seconds since the beginning of the match.
  , delta :: F32.F32
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
  F32.bitPut (time frame)
  F32.bitPut (delta frame)
  Replication.putReplications (replications frame)

decodeFramesBits
  :: (Int, Int, Int)
  -> Int
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
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
  -> State.StateT (Map.Map CompressedWord.CompressedWord U32.U32) BitGet Frame
bitGet version limit classes =
  Frame
    <$> Trans.lift F32.bitGet
    <*> Trans.lift F32.bitGet
    <*> Replication.decodeReplicationsBits version limit classes
