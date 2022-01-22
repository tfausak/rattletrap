module Rattletrap.Type.Header where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.Property as Property
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

-- | Contains high-level metadata about a 'Rattletrap.Replay.Replay'.
data Header = Header
  { version :: Version.Version
  , label :: Str.Str
  -- ^ Always @TAGame.Replay_Soccar_TA@.
  , properties :: Dictionary.Dictionary Property.Property
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

instance Argo.HasCodec Header where
  codec =
    Argo.map
        (\(a, b, c, d, e) -> Header
          { version = Version.Version a b c
          , label = d
          , properties = e
          }
        )
        (\x ->
          ( Version.major $ version x
          , Version.minor $ version x
          , Version.patch $ version x
          , label x
          , properties x
          )
        )
      . Argo.fromObjectCodec Argo.Allow
      $ (,,,,)
      <$> Argo.project
            (\(x, _, _, _, _) -> x)
            (Argo.required (Argo.fromString "engine_version") Argo.codec)
      <*> Argo.project
            (\(_, x, _, _, _) -> x)
            (Argo.required (Argo.fromString "licensee_version") Argo.codec)
      <*> Argo.project
            (\(_, _, x, _, _) -> x)
            (Argo.optional (Argo.fromString "patch_version") Argo.codec)
      <*> Argo.project
            (\(_, _, _, x, _) -> x)
            (Argo.required (Argo.fromString "label") Argo.codec)
      <*> Argo.project
            (\(_, _, _, _, x) -> x)
            (Argo.required (Argo.fromString "properties") Argo.codec)

bytePut :: Header -> BytePut.BytePut
bytePut x =
  Version.bytePut (version x) <> Str.bytePut (label x) <> Dictionary.bytePut
    Property.bytePut
    (properties x)

byteGet :: ByteGet.ByteGet Header
byteGet = ByteGet.label "Header" $ do
  version <- ByteGet.label "version" Version.byteGet
  label <- ByteGet.label "label" Str.byteGet
  properties <- ByteGet.label "properties"
    $ Dictionary.byteGet Property.byteGet
  pure Header { version, label, properties }
