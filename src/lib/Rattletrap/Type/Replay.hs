module Rattletrap.Type.Replay where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.Header as Header
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Property as Property
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

type Replay = ReplayWith (Section.Section Header.Header) (Section.Section Content.Content)

-- | A Rocket League replay.
data ReplayWith header content = Replay
  { header :: header
  -- ^ This has most of the high-level metadata.
  , content :: content
  -- ^ This has most of the low-level game data.
  }
  deriving (Eq, Show)

$(deriveJson ''ReplayWith)

schema :: Schema.Schema
schema = Schema.named "replay" $ Schema.object
  [ (Json.pair "header" . Schema.json $ Section.schema Header.schema, True)
  , (Json.pair "content" . Schema.json $ Section.schema Content.schema, True)
  ]

bytePut :: Replay -> BytePut.BytePut
bytePut x = Section.bytePut Header.bytePut (header x)
  <> Section.bytePut Content.bytePut (content x)

byteGet :: Bool -> Bool -> ByteGet.ByteGet Replay
byteGet fast skip = do
  hs <- Section.byteGet skip $ ByteGet.byteString . fromIntegral . U32.toWord32
  h <- either fail pure . ByteGet.run Header.byteGet $ Section.body hs
  cs <- Section.byteGet skip $ ByteGet.byteString . fromIntegral . U32.toWord32
  c <- if fast
    then pure Content.empty
    else either fail pure . ByteGet.run (getContent h) $ Section.body cs
  pure Replay
    { header = hs { Section.body = h }
    , content = cs { Section.body = c }
    }

getContent :: Header.Header -> ByteGet.ByteGet Content.Content
getContent h =
  Content.byteGet (getVersion h) (getNumFrames h) (getMaxChannels h)

getVersion :: Header.Header -> Version.Version
getVersion x = Version.Version
  { Version.major = fromIntegral . U32.toWord32 $ Header.engineVersion x
  , Version.minor = fromIntegral . U32.toWord32 $ Header.licenseeVersion x
  , Version.patch = getPatchVersion x
  }

getPatchVersion :: Header.Header -> Int
getPatchVersion header_ = case Header.patchVersion header_ of
  Just version -> fromIntegral (U32.toWord32 version)
  Nothing ->
    case
        Dictionary.lookup
          (Str.fromString "MatchType")
          (Header.properties header_)
      of
      -- This is an ugly, ugly hack to handle replays from season 2 of RLCS.
      -- See `decodeSpawnedReplicationBits` and #85.
        Just Property.Property { Property.value = PropertyValue.Name str }
          | Str.toString str == "Lan" -> -1
        _ -> 0

getNumFrames :: Header.Header -> Int
getNumFrames header_ =
  case
      Dictionary.lookup
        (Str.fromString "NumFrames")
        (Header.properties header_)
    of
      Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
        fromIntegral (I32.toInt32 numFrames)
      _ -> 0

getMaxChannels :: Header.Header -> Word
getMaxChannels header_ =
  case
      Dictionary.lookup
        (Str.fromString "MaxChannels")
        (Header.properties header_)
    of
      Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
        fromIntegral (I32.toInt32 numFrames)
      _ -> 1023
