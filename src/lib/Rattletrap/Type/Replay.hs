{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replay where

import Rattletrap.Type.Common
import Rattletrap.Type.Content
import Rattletrap.Type.Header
import Rattletrap.Type.Section
import Rattletrap.Decode.Common
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Int32le
import Rattletrap.Type.Property
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Binary as Binary

type FullReplay = Replay Content

-- | A Rocket League replay.
data Replay content = Replay
  { replayHeader :: Section Header
  -- ^ This has most of the high-level metadata.
  , replayContent :: Section content
  -- ^ This has most of the low-level game data.
  }
  deriving (Eq, Show)

$(deriveJson ''Replay)

-- | Generates a raw replay. Use this with 'Data.Binary.Put.runPut'.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putReplay' replay)
-- @
putReplay :: FullReplay -> Binary.Put
putReplay replay = do
  putSection putHeader (replayHeader replay)
  putSection putContent (replayContent replay)

decodeReplay :: Bool -> ByteGet FullReplay
decodeReplay fast = do
  header <- decodeSection decodeHeader
  content <- if fast
    then pure $ toSection putContent defaultContent
    else
      let body = sectionBody header
      in
        decodeSection $ decodeContent
          (getVersion body)
          (getNumFrames body)
          (getMaxChannels body)
  pure $ Replay header content

getVersion :: Header -> (Int, Int, Int)
getVersion header =
  ( fromIntegral (word32leValue (headerEngineVersion header))
  , fromIntegral (word32leValue (headerLicenseeVersion header))
  , getPatchVersion header
  )

getPatchVersion :: Header -> Int
getPatchVersion header = case headerPatchVersion header of
  Just version -> fromIntegral (word32leValue version)
  Nothing ->
    case dictionaryLookup (toStr "MatchType") (headerProperties header) of
      -- This is an ugly, ugly hack to handle replays from season 2 of RLCS.
      -- See `decodeSpawnedReplicationBits` and #85.
      Just Property { propertyValue = PropertyValueName str }
        | fromStr str == "Lan" -> -1
      _ -> 0

getNumFrames :: Header -> Int
getNumFrames header =
  case dictionaryLookup (toStr "NumFrames") (headerProperties header) of
    Just (Property _ _ (PropertyValueInt numFrames)) ->
      fromIntegral (int32leValue numFrames)
    _ -> 0

getMaxChannels :: Header -> Word
getMaxChannels header =
  case dictionaryLookup (toStr "MaxChannels") (headerProperties header) of
    Just (Property _ _ (PropertyValueInt numFrames)) ->
      fromIntegral (int32leValue numFrames)
    _ -> 1023
