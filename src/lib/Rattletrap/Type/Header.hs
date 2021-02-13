{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Header where

import Rattletrap.Type.Common
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Property
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

-- | Contains high-level metadata about a 'Rattletrap.Replay.Replay'.
data Header = Header
  { headerEngineVersion :: Word32le
  -- ^ The "major" ("engine") version number.
  , headerLicenseeVersion :: Word32le
  -- ^ The "minor" ("licensee") version number.
  , headerPatchVersion :: Maybe Word32le
  -- ^ The "patch" ("net") version number.
  , headerLabel :: Str
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
  }
  deriving (Eq, Show)

$(deriveJson ''Header)

putHeader :: Header -> BytePut
putHeader header = do
  putWord32 (headerEngineVersion header)
  putWord32 (headerLicenseeVersion header)
  case headerPatchVersion header of
    Nothing -> pure ()
    Just patchVersion -> putWord32 patchVersion
  putText (headerLabel header)
  putDictionary putProperty (headerProperties header)

decodeHeader :: ByteGet Header
decodeHeader = do
  (major, minor) <- (,) <$> decodeWord32le <*> decodeWord32le
  Header major minor
    <$> decodeWhen
          (word32leValue major >= 868 && word32leValue minor >= 18)
          decodeWord32le
    <*> decodeStr
    <*> decodeDictionary decodeProperty
