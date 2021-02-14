{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replay where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.Type.Header as Header
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Property as Property
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.ByteGet as ByteGet

type FullReplay = Replay Content.Content

-- | A Rocket League replay.
data Replay content = Replay
  { header :: Section.Section Header.Header
  -- ^ This has most of the high-level metadata.
  , content :: Section.Section content
  -- ^ This has most of the low-level game data.
  }
  deriving (Eq, Show)

$(deriveJson ''Replay)

bytePut :: FullReplay -> BytePut.BytePut
bytePut replay = do
  Section.bytePut Header.putHeader (header replay)
  Section.bytePut Content.bytePut (content replay)

byteGet :: Bool -> ByteGet.ByteGet FullReplay
byteGet fast = do
  header_ <- Section.byteGet Header.decodeHeader
  content_ <- if fast
    then pure $ Section.create Content.bytePut Content.empty
    else
      let body = Section.body header_
      in
        Section.byteGet $ Content.byteGet
          (getVersion body)
          (getNumFrames body)
          (getMaxChannels body)
  pure $ Replay header_ content_

getVersion :: Header.Header -> (Int, Int, Int)
getVersion header_ =
  ( fromIntegral (U32.toWord32 (Header.engineVersion header_))
  , fromIntegral (U32.toWord32 (Header.licenseeVersion header_))
  , getPatchVersion header_
  )

getPatchVersion :: Header.Header -> Int
getPatchVersion header_ = case Header.patchVersion header_ of
  Just version -> fromIntegral (U32.toWord32 version)
  Nothing ->
    case Dictionary.lookup (Str.fromString "MatchType") (Header.properties header_) of
      -- This is an ugly, ugly hack to handle replays from season 2 of RLCS.
      -- See `decodeSpawnedReplicationBits` and #85.
      Just Property.Property { Property.value = PropertyValue.Name str }
        | Str.toString str == "Lan" -> -1
      _ -> 0

getNumFrames :: Header.Header -> Int
getNumFrames header_ =
  case Dictionary.lookup (Str.fromString "NumFrames") (Header.properties header_) of
    Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
      fromIntegral (I32.toInt32 numFrames)
    _ -> 0

getMaxChannels :: Header.Header -> Word
getMaxChannels header_ =
  case Dictionary.lookup (Str.fromString "MaxChannels") (Header.properties header_) of
    Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
      fromIntegral (I32.toInt32 numFrames)
    _ -> 1023
