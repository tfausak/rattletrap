{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replay where

import Rattletrap.Type.Common
import Rattletrap.Type.Content
import Rattletrap.Type.Header
import qualified Rattletrap.Type.Section as Section
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Int32le
import qualified Rattletrap.Type.Property as Property
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le

type FullReplay = Replay Content

-- | A Rocket League replay.
data Replay content = Replay
  { header :: Section.Section Header
  -- ^ This has most of the high-level metadata.
  , content :: Section.Section content
  -- ^ This has most of the low-level game data.
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Replay jsonOptions)

-- | Generates a raw replay. Use this with 'Data.BytePut.runPut'.
--
-- @
-- let bytes = 'Data.BytePut.runPut' ('bytePut' replay)
-- @
bytePut :: FullReplay -> BytePut
bytePut replay = do
  Section.bytePut putHeader (header replay)
  Section.bytePut putContent (content replay)

byteGet :: Bool -> ByteGet FullReplay
byteGet fast = do
  header_ <- Section.byteGet decodeHeader
  content_ <- if fast
    then pure $ Section.create putContent defaultContent
    else
      let body = Section.body header_
      in
        Section.byteGet $ decodeContent
          (getVersion body)
          (getNumFrames body)
          (getMaxChannels body)
  pure $ Replay header_ content_

getVersion :: Header -> (Int, Int, Int)
getVersion header_ =
  ( fromIntegral (Word32le.toWord32 (headerEngineVersion header_))
  , fromIntegral (Word32le.toWord32 (headerLicenseeVersion header_))
  , getPatchVersion header_
  )

getPatchVersion :: Header -> Int
getPatchVersion header_ = case headerPatchVersion header_ of
  Just version -> fromIntegral (Word32le.toWord32 version)
  Nothing ->
    case dictionaryLookup (Str.fromString "MatchType") (headerProperties header_) of
      -- This is an ugly, ugly hack to handle replays from season 2 of RLCS.
      -- See `decodeSpawnedReplicationBits` and #85.
      Just Property.Property { Property.value = PropertyValue.Name str }
        | Str.toString str == "Lan" -> -1
      _ -> 0

getNumFrames :: Header -> Int
getNumFrames header_ =
  case dictionaryLookup (Str.fromString "NumFrames") (headerProperties header_) of
    Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
      fromIntegral (int32leValue numFrames)
    _ -> 0

getMaxChannels :: Header -> Word
getMaxChannels header_ =
  case dictionaryLookup (Str.fromString "MaxChannels") (headerProperties header_) of
    Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
      fromIntegral (int32leValue numFrames)
    _ -> 1023
