module Rattletrap.Type.Frame where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Replication as Replication
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Frame = Frame
  { time :: F32.F32
  -- ^ Time in seconds since the beginning of the match.
  , delta :: F32.F32
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , replications :: List.List Replication.Replication
  }
  deriving (Eq, Show)

instance Argo.HasCodec Frame where
  codec = Argo.identified .
    Argo.fromObjectCodec Argo.Allow
      $ Frame
      <$> Argo.project time (Argo.required (Argo.fromString "time") Argo.codec)
      <*> Argo.project
            delta
            (Argo.required (Argo.fromString "delta") Argo.codec)
      <*> Argo.project
            replications
            (Argo.required (Argo.fromString "replications") Argo.codec)

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
  -> BitGet.BitGet (List.List Frame)
decodeFramesBits matchType version count limit classes =
  fmap snd $ decodeFramesBitsWith
    matchType
    version
    count
    limit
    classes
    Map.empty
    0
    []

decodeFramesBitsWith
  :: Maybe Str.Str
  -> Version.Version
  -> Int
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> Int
  -> [Frame]
  -> BitGet.BitGet
       ( Map.Map CompressedWord.CompressedWord U32.U32
       , List.List Frame
       )
decodeFramesBitsWith matchType version count limit classes actorMap index frames
  = if index >= count
    then pure (actorMap, List.fromList $ reverse frames)
    else do
      (newActorMap, frame) <-
        BitGet.label ("element (" <> show index <> ")")
          $ bitGet matchType version limit classes actorMap
      decodeFramesBitsWith
          matchType
          version
          count
          limit
          classes
          newActorMap
          (index + 1)
        $ frame
        : frames

bitGet
  :: Maybe Str.Str
  -> Version.Version
  -> Word
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> BitGet.BitGet
       (Map.Map CompressedWord.CompressedWord U32.U32, Frame)
bitGet matchType version limit classes actorMap = BitGet.label "Frame" $ do
  time <- BitGet.label "time" F32.bitGet
  delta <- BitGet.label "delta" F32.bitGet
  (newActorMap, replications) <-
    BitGet.label "replications" $ Replication.decodeReplicationsBits
      matchType
      version
      limit
      classes
      actorMap
  pure (newActorMap, Frame { time, delta, replications })
