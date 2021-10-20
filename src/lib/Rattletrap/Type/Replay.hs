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
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Version as Version

type Replay
  = ReplayWith
      (Section.Section Header.Header)
      (Section.Section Content.Content)

-- | A Rocket League replay.
data ReplayWith header content = Replay
  { header :: header
  -- ^ This has most of the high-level metadata.
  , content :: content
  -- ^ This has most of the low-level game data.
  }
  deriving (Eq, Show)

instance (Json.FromValue h, Json.FromValue c) => Json.FromValue (ReplayWith h c) where
  fromValue = Json.withObject "Replay" $ \object -> do
    header <- Json.required object "header"
    content <- Json.required object "content"
    pure Replay { header, content }

instance (Json.ToValue h, Json.ToValue c) => Json.ToValue (ReplayWith h c) where
  toValue x = Json.object
    [ Json.pair "$schema" schemaUrl
    , Json.pair "header" $ header x
    , Json.pair "content" $ content x
    ]

schema :: Schema.Schema -> Schema.Schema -> Schema.Schema
schema h c = Schema.named "replay" $ Schema.object
  [ (Json.pair "header" $ Schema.ref h, True)
  , (Json.pair "content" $ Schema.ref c, True)
  ]

schemaUrl :: String
schemaUrl = mconcat
  [ "https://github.com/tfausak/rattletrap/releases/download/"
  , Version.string
  , "/rattletrap-"
  , Version.string
  , "-schema.json"
  ]

bytePut :: Replay -> BytePut.BytePut
bytePut x = Section.bytePut Header.bytePut (header x)
  <> Section.bytePut Content.bytePut (content x)

byteGet :: Bool -> Bool -> ByteGet.ByteGet Replay
byteGet fast skip = ByteGet.label "Replay" $ do
  header <- ByteGet.label "header" $ do
    section <-
      Section.byteGet skip $ ByteGet.byteString . fromIntegral . U32.toWord32
    body <- ByteGet.embed Header.byteGet $ Section.body section
    pure section { Section.body }
  content <- ByteGet.label "content" $ do
    section <-
      Section.byteGet skip $ ByteGet.byteString . fromIntegral . U32.toWord32
    body <- if fast
      then pure Content.empty
      else ByteGet.embed (getContent $ Section.body header)
        $ Section.body section
    pure section { Section.body }
  pure Replay { header, content }

getContent :: Header.Header -> ByteGet.ByteGet Content.Content
getContent h = Content.byteGet
  (getMatchType h)
  (Header.version h)
  (getNumFrames h)
  (getMaxChannels h)

getMatchType :: Header.Header -> Maybe Str.Str
getMatchType header = do
  Property.Property { Property.value } <-
    Dictionary.lookup (Str.fromString "MatchType") $ Header.properties header
  case value of
    PropertyValue.Name x -> Just $ Property.Name.toStr x
    _ -> Nothing

getNumFrames :: Header.Header -> Int
getNumFrames header_ =
  case
      Dictionary.lookup
        (Str.fromString "NumFrames")
        (Header.properties header_)
    of
      Just (Property.Property _ _ (PropertyValue.Int numFrames)) ->
        fromIntegral (I32.toInt32 (Property.Int.toI32 numFrames))
      _ -> 0

getMaxChannels :: Header.Header -> Word
getMaxChannels header_ =
  case
      Dictionary.lookup
        (Str.fromString "MaxChannels")
        (Header.properties header_)
    of
      Just (Property.Property _ _ (PropertyValue.Int maxChannels)) ->
        fromIntegral (I32.toInt32 (Property.Int.toI32 maxChannels))
      _ -> 1023
