module Rattletrap.Type.Frame where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Replication as Replication
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

data Frame = Frame
  { time :: F32.F32
  -- ^ Time in seconds since the beginning of the match.
  , delta :: F32.F32
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , replications :: List.List Replication.Replication
  }
  deriving (Eq, Show)

instance Json.FromJSON Frame where
  parseJSON = Json.withObject "Frame" $ \object -> do
    time <- Json.required object "time"
    delta <- Json.required object "delta"
    replications <- Json.required object "replications"
    pure Frame { time, delta, replications }

instance Json.ToJSON Frame where
  toJSON x = Json.object
    [ Json.pair "time" $ time x
    , Json.pair "delta" $ delta x
    , Json.pair "replications" $ replications x
    ]

schema :: Schema.Schema
schema = Schema.named "frame" $ Schema.object
  [ (Json.pair "time" $ Schema.ref F32.schema, True)
  , (Json.pair "delta" $ Schema.ref F32.schema, True)
  , ( Json.pair "replications" . Schema.json $ List.schema Replication.schema
    , True
    )
  ]

putFrames :: List.List Frame -> BitPut.BitPut
putFrames = foldMap bitPut . List.toList

bitPut :: Frame -> BitPut.BitPut
bitPut frame =
  F32.bitPut (time frame)
    <> F32.bitPut (delta frame)
    <> Replication.putReplications (replications frame)

decodeFramesBits
  :: Maybe Str.Str
  -> Version.Version
  -> Int
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       (List.List Frame)
decodeFramesBits matchType version count limit classes =
  List.replicateM count $ bitGet matchType version limit classes

bitGet
  :: Maybe Str.Str
  -> Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       Frame
bitGet matchType version limit classes = do
  time <- Trans.lift $ BitGet.label "time" F32.bitGet
  delta <- Trans.lift F32.bitGet
  replications <- Replication.decodeReplicationsBits
    matchType
    version
    limit
    classes
  pure Frame { time, delta, replications }
