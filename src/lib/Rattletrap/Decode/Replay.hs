module Rattletrap.Decode.Replay
  ( decodeReplay
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Content
import Rattletrap.Decode.Header
import Rattletrap.Decode.Section
import Rattletrap.Type.Content
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Header
import Rattletrap.Type.Int32le
import Rattletrap.Type.Property
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Replay
import Rattletrap.Type.Section
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

decodeReplay :: Bool -> Decode FullReplay
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
