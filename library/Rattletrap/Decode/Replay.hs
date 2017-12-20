module Rattletrap.Decode.Replay
  ( decodeReplay
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Content
import Rattletrap.Decode.Header
import Rattletrap.Decode.Section
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Header
import Rattletrap.Type.Int32le
import Rattletrap.Type.Property
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Replay
import Rattletrap.Type.Section
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

decodeReplay :: Decode Replay
decodeReplay = do
  header <- decodeSection decodeHeader
  Replay header <$> decodeSection
    ( decodeContent
      (getVersion (sectionBody header))
      (getNumFrames (sectionBody header))
      (getMaxChannels (sectionBody header))
    )

getVersion :: Header -> (Int, Int, Int)
getVersion header =
  ( fromIntegral (word32leValue (headerEngineVersion header))
  , fromIntegral (word32leValue (headerLicenseeVersion header))
  , maybe 0 (fromIntegral . word32leValue) (headerPatchVersion header)
  )

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
