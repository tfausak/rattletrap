module Rattletrap.Type.Replay where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.Header as Header
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Property as Property
import qualified Rattletrap.Type.Property.Int as Property.Int
import qualified Rattletrap.Type.Property.Name as Property.Name
import qualified Rattletrap.Type.Property.Str as Property.Str
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Version as Version

type Replay =
  ReplayWith
    (Section.Section Header.Header)
    (Section.Section Content.Content)

-- | A Rocket League replay.
data ReplayWith header content = Replay
  { -- | This has most of the high-level metadata.
    header :: header,
    -- | This has most of the low-level game data.
    content :: content
  }
  deriving (Eq, Show)

instance (Json.FromJSON h, Json.FromJSON c) => Json.FromJSON (ReplayWith h c) where
  parseJSON = Json.withObject "Replay" $ \object -> do
    header <- Json.required object "header"
    content <- Json.required object "content"
    pure Replay {header, content}

instance (Json.ToJSON h, Json.ToJSON c) => Json.ToJSON (ReplayWith h c) where
  toJSON x =
    Json.object
      [ Json.pair "$schema" schemaUrl,
        Json.pair "header" $ header x,
        Json.pair "content" $ content x
      ]

schema :: Schema.Schema -> Schema.Schema -> Schema.Schema
schema h c =
  Schema.named "replay" $
    Schema.object
      [ (Json.pair "header" $ Schema.ref h, True),
        (Json.pair "content" $ Schema.ref c, True)
      ]

schemaUrl :: String
schemaUrl =
  mconcat
    [ "https://github.com/tfausak/rattletrap/releases/download/",
      Version.string,
      "/rattletrap-",
      Version.string,
      "-schema.json"
    ]

bytePut :: Bool -> Replay -> BytePut.BytePut
bytePut fast x =
  Section.bytePut Header.bytePut (header x)
    <> Section.bytePut (Content.bytePut fast) (content x)

byteGet :: Bool -> Bool -> ByteGet.ByteGet Replay
byteGet fast skip = ByteGet.label "Replay" $ do
  header <- ByteGet.label "header" $ do
    section <-
      Section.byteGet skip $ ByteGet.byteString . fromIntegral . U32.toWord32
    body <- ByteGet.embed Header.byteGet $ Section.body section
    pure section {Section.body}
  content <- ByteGet.label "content" $ do
    section <-
      Section.byteGet skip $ ByteGet.byteString . fromIntegral . U32.toWord32
    body <-
      ByteGet.embed (getContent fast $ Section.body header) $
        Section.body section
    pure section {Section.body}
  pure Replay {header, content}

getContent :: Bool -> Header.Header -> ByteGet.ByteGet Content.Content
getContent fast h =
  Content.byteGet
    fast
    (getMatchType h)
    (Header.version h)
    (getNumFrames h)
    (getMaxChannels h)
    (getBuildVersion h)

getMatchType :: Header.Header -> Maybe Str.Str
getMatchType header = do
  Property.Property {Property.value} <-
    Dictionary.lookup (Str.fromString "MatchType") $ Header.properties header
  case value of
    PropertyValue.Name x -> Just $ Property.Name.toStr x
    _ -> Nothing

getNumFrames :: Header.Header -> Int
getNumFrames header_ =
  case Dictionary.lookup
    (Str.fromString "NumFrames")
    (Header.properties header_) of
    Just (Property.Property _ _ _ (PropertyValue.Int numFrames)) ->
      fromIntegral (I32.toInt32 (Property.Int.toI32 numFrames))
    _ -> 0

getMaxChannels :: Header.Header -> Word
getMaxChannels header_ =
  subtract 1 $
    case Dictionary.lookup
      (Str.fromString "MaxChannels")
      (Header.properties header_) of
      Just (Property.Property _ _ _ (PropertyValue.Int maxChannels)) ->
        fromIntegral (I32.toInt32 (Property.Int.toI32 maxChannels))
      _ -> 1023

getBuildVersion :: Header.Header -> Maybe Str.Str
getBuildVersion header = do
  property <-
    Dictionary.lookup (Str.fromString "BuildVersion") $
      Header.properties header
  case Property.value property of
    PropertyValue.Str x -> Just $ Property.Str.toStr x
    _ -> Nothing
