module Rattletrap.Header where

import Rattletrap.Primitive
import Rattletrap.Property
import Rattletrap.PropertyValue

import qualified Data.Binary as Binary
import qualified Data.Map as Map

-- | Contains high-level metadata about a 'Rattletrap.Replay.Replay'.
data Header = Header
  { headerEngineVersion :: Word32
  -- ^ The "major" version number.
  , headerLicenseeVersion :: Word32
  -- ^ The "minor" version number.
  , headerPatchVersion :: Maybe Word32
  -- ^ The "patch" version number.
  , headerLabel :: Text
  -- ^ Always @TAGame.Replay_Soccar_TA@.
  , headerProperties :: Dictionary Property
  -- ^ These properties determine how a replay will look in the list of
  -- replays in-game. One element is required for the replay to show up:
  --
  -- - MapName: This is a 'Rattletrap.PropertyValue.NameProperty' with a
  --   case-insensitive map identifier, like @Stadium_P@.
  --
  -- There are many other properties that affect how the replay looks in the
  -- list of replays.
  --
  -- - Date: A 'Rattletrap.PropertyValue.StrProperty' with the format
  --   @YYYY-mm-dd:HH-MM@. Dates are not validated, but the month must be
  --   between 1 and 12 to show up. The hour is shown modulo 12 with AM or PM.
  -- - MatchType: A 'Rattletrap.PropertyValue.NameProperty'. If this is not
  --   one of the expected values, nothing will be shown next to the replay's
  --   map. The expected values are: @Online@, @Offline@, @Private@, and
  --   @Season@.
  -- - NumFrames: This 'Rattletrap.PropertyValue.IntProperty' is used to
  --   calculate the length of the match. There are 30 frames per second,
  --   a typical 5-minute match has about 9,000 frames.
  -- - PrimaryPlayerTeam: This is an 'Rattletrap.PropertyValue.IntProperty'.
  --   It is either 0 (blue) or 1 (orange). Any other value is ignored. If
  --   this would be 0, you don't have to set it at all.
  -- - ReplayName: An optional 'Rattletrap.PropertyValue.StrProperty' with a
  --   user-supplied name for the replay.
  -- - Team0Score: The blue team's score as an
  --   'Rattletrap.PropertyValue.IntProperty'. Can be omitted if the score is
  --   0.
  -- - Team1Score: The orange team's score as an
  --   'Rattletrap.PropertyValue.IntProperty'. Can also be omitted if the
  --   score is 0.
  -- - TeamSize: An 'Rattletrap.PropertyValue.IntProperty' with the number of
  --   players per team. This value is not validated, so you can put absurd
  --   values like 99. To get an "unfair" team size like 1v4, you must set the
  --   bUnfairBots 'Rattletrap.PropertyValue.BoolProperty' to @True@.
  } deriving (Eq, Ord, Show)

getHeader :: Binary.Get Header
getHeader = do
  engineVersion <- getWord32
  licenseeVersion <- getWord32
  patchVersion <- getPatchVersion engineVersion licenseeVersion
  label <- getText
  properties <- getDictionary getProperty
  pure (Header engineVersion licenseeVersion patchVersion label properties)

getPatchVersion :: Word32 -> Word32 -> Binary.Get (Maybe Word32)
getPatchVersion major minor = if hasPatchVersion major minor
  then do
    patchVersion <- getWord32
    pure (Just patchVersion)
  else pure Nothing

hasPatchVersion :: Word32 -> Word32 -> Bool
hasPatchVersion major minor = major >= Word32 868 && minor >= Word32 18

putHeader :: Header -> Binary.Put
putHeader header = do
  putWord32 (headerEngineVersion header)
  putWord32 (headerLicenseeVersion header)
  case headerPatchVersion header of
    Nothing -> pure ()
    Just patchVersion -> putWord32 patchVersion
  putText (headerLabel header)
  putDictionary putProperty (headerProperties header)

getVersion :: Header -> (Int, Int)
getVersion header =
  let
    major = getMajorVersion header
    minor = getMinorVersion header
  in (major, minor)

getMajorVersion :: Header -> Int
getMajorVersion header =
  fromIntegral (word32Value (headerEngineVersion header))

getMinorVersion :: Header -> Int
getMinorVersion header =
  fromIntegral (word32Value (headerLicenseeVersion header))

getNumFrames :: Header -> Int
getNumFrames header =
  let
    key = textValue (stringToText "NumFrames")
    properties = dictionaryValue (headerProperties header)
  in case Map.lookup key properties of
    Just (Property _ _ (IntProperty numFrames)) ->
      fromIntegral (int32Value numFrames)
    _ -> 0

getMaxChannels :: Header -> Word
getMaxChannels header =
  let
    key = textValue (stringToText "MaxChannels")
    properties = dictionaryValue (headerProperties header)
  in case Map.lookup key properties of
    Just (Property _ _ (IntProperty numFrames)) ->
      fromIntegral (int32Value numFrames)
    _ -> 1023
