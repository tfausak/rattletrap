module Rattletrap.Type.Content where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Cache as Cache
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.ClassMapping as ClassMapping
import qualified Rattletrap.Type.Frame as Frame
import qualified Rattletrap.Type.Keyframe as Keyframe
import qualified Rattletrap.Type.List as RList
import qualified Rattletrap.Type.Mark as Mark
import qualified Rattletrap.Type.Message as Message
import qualified Rattletrap.Type.Name as Name
import qualified Rattletrap.Type.ObjectName as ObjectName
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Bytes as Bytes
import qualified Rattletrap.Utility.Json as Json

type Content = ContentWith (RList.List Frame.Frame)

-- | Contains low-level game data about a 'Rattletrap.Replay.Replay'.
data ContentWith frames = Content
  { -- | This typically only has one element, like @stadium_oob_audio_map@.
    levels :: RList.List Str.Str,
    -- | A list of which frames are key frames. Although they aren't necessary
    -- for replay, key frames are frames that replicate every actor. They
    -- typically happen once every 10 seconds.
    keyframes :: RList.List Keyframe.Keyframe,
    -- | The size of the stream in bytes. This is only really necessary because
    -- the stream has some arbitrary amount of padding at the end.
    streamSize :: U32.U32,
    -- | The actual game data. This is where all the interesting information is.
    frames :: frames,
    -- | Debugging messages. In newer replays, this is always empty.
    messages :: RList.List Message.Message,
    -- | Tick marks shown on the scrubber when watching a replay.
    marks :: RList.List Mark.Mark,
    -- | A list of @.upk@ files to load, like
    -- @..\\..\\TAGame\\CookedPCConsole\\Stadium_P.upk@.
    packages :: RList.List Str.Str,
    -- | Objects in the stream. Used for the
    -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
    objects :: RList.List ObjectName.ObjectName,
    -- | It's not clear what these are used for. This list is usually not empty,
    -- but appears unused otherwise.
    names :: RList.List Name.Name,
    -- | A mapping between classes and their ID in the stream. Used for the
    -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
    classMappings :: RList.List ClassMapping.ClassMapping,
    -- | A list of classes along with their parent classes and attributes. Used
    -- for the 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
    caches :: RList.List Cache.Cache,
    unknown :: [Word.Word8]
  }
  deriving (Eq, Show)

instance (Json.FromJSON frames) => Json.FromJSON (ContentWith frames) where
  parseJSON = Json.withObject "Content" $ \object -> do
    levels <- Json.required object "levels"
    keyframes <- Json.required object "key_frames"
    streamSize <- Json.required object "stream_size"
    frames <- Json.required object "frames"
    messages <- Json.required object "messages"
    marks <- Json.required object "marks"
    packages <- Json.required object "packages"
    objects <- Json.required object "objects"
    names <- Json.required object "names"
    classMappings <- Json.required object "class_mappings"
    caches <- Json.required object "caches"
    unknown <- Json.required object "unknown"
    pure
      Content
        { levels,
          keyframes,
          streamSize,
          frames,
          messages,
          marks,
          packages,
          objects,
          names,
          classMappings,
          caches,
          unknown
        }

instance (Json.ToJSON frames) => Json.ToJSON (ContentWith frames) where
  toJSON x =
    Json.object
      [ Json.pair "levels" $ levels x,
        Json.pair "key_frames" $ keyframes x,
        Json.pair "stream_size" $ streamSize x,
        Json.pair "frames" $ frames x,
        Json.pair "messages" $ messages x,
        Json.pair "marks" $ marks x,
        Json.pair "packages" $ packages x,
        Json.pair "objects" $ objects x,
        Json.pair "names" $ names x,
        Json.pair "class_mappings" $ classMappings x,
        Json.pair "caches" $ caches x,
        Json.pair "unknown" $ unknown x
      ]

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named "content" $
    Schema.object
      [ (Json.pair "levels" . Schema.json $ RList.schema Str.schema, True),
        (Json.pair "key_frames" . Schema.json $ RList.schema Keyframe.schema, True),
        (Json.pair "stream_size" $ Schema.ref U32.schema, True),
        (Json.pair "frames" $ Schema.json s, True),
        (Json.pair "messages" . Schema.json $ RList.schema Message.schema, True),
        (Json.pair "marks" . Schema.json $ RList.schema Mark.schema, True),
        (Json.pair "packages" . Schema.json $ RList.schema Str.schema, True),
        (Json.pair "objects" . Schema.json $ RList.schema Str.schema, True),
        (Json.pair "names" . Schema.json $ RList.schema Str.schema, True),
        ( Json.pair "class_mappings" . Schema.json $
            RList.schema
              ClassMapping.schema,
          True
        ),
        (Json.pair "caches" . Schema.json $ RList.schema Cache.schema, True),
        (Json.pair "unknown" . Schema.json $ Schema.array U8.schema, True)
      ]

empty :: Content
empty =
  Content
    { levels = RList.empty,
      keyframes = RList.empty,
      streamSize = U32.fromWord32 0,
      frames = RList.empty,
      messages = RList.empty,
      marks = RList.empty,
      packages = RList.empty,
      objects = RList.empty,
      names = RList.empty,
      classMappings = RList.empty,
      caches = RList.empty,
      unknown = []
    }

bytePut :: Bool -> Content -> BytePut.BytePut
bytePut fast x =
  RList.bytePut Str.bytePut (levels x)
    <> RList.bytePut Keyframe.bytePut (keyframes x)
    <> putFrames x {frames = if fast then RList.empty else frames x}
    <> RList.bytePut Message.bytePut (messages x)
    <> RList.bytePut Mark.bytePut (marks x)
    <> RList.bytePut Str.bytePut (packages x)
    <> RList.bytePut ObjectName.bytePut (objects x)
    <> RList.bytePut Name.bytePut (names x)
    <> RList.bytePut ClassMapping.bytePut (classMappings x)
    <> RList.bytePut Cache.bytePut (caches x)
    <> foldMap BytePut.word8 (unknown x)

putFrames :: Content -> BytePut.BytePut
putFrames x =
  let stream =
        BytePut.toByteString . BitPut.toBytePut . Frame.putFrames $ frames x
      -- This is a little strange. When parsing a binary replay, the stream size
      -- is given before the stream itself. When generating the JSON, the stream
      -- size is included. That allows a bit-for-bit identical binary replay to
      -- be generated from the JSON. However if you modify the JSON before
      -- converting it back into binary, the stream size might be different.
      --
      -- If it was possible to know how much padding the stream required without
      -- carrying it along as extra data on the side, this logic could go away.
      -- Unforunately that isn't currently known. See this issue for details:
      -- <https://github.com/tfausak/rattletrap/issues/171>.
      expectedStreamSize = streamSize x
      actualStreamSize =
        U32.fromWord32 . fromIntegral $ ByteString.length stream
      streamSize_ =
        U32.fromWord32 $
          max (U32.toWord32 expectedStreamSize) (U32.toWord32 actualStreamSize)
   in U32.bytePut streamSize_
        <> BytePut.byteString (Bytes.padBytes (U32.toWord32 streamSize_) stream)

byteGet ::
  Bool ->
  Maybe Str.Str ->
  -- | Version numbers, usually from 'Rattletrap.Header.getVersion'.
  Version.Version ->
  -- | The number of frames in the stream, usually from
  -- 'Rattletrap.Header.getNumFrames'.
  Int ->
  -- | The maximum number of channels in the stream, usually from
  -- 'Rattletrap.Header.getMaxChannels'.
  Word ->
  -- | 'Rattletrap.Header.getBuildVersion'
  Maybe Str.Str ->
  ByteGet.ByteGet Content
byteGet fast matchType version numFrames maxChannels buildVersion =
  ByteGet.label "Content" $ do
    levels <- ByteGet.label "levels" $ RList.byteGet Str.byteGet
    keyframes <- ByteGet.label "keyframes" $ RList.byteGet Keyframe.byteGet
    streamSize <- ByteGet.label "streamSize" U32.byteGet
    stream <-
      ByteGet.label "stream" . ByteGet.byteString . fromIntegral $
        U32.toWord32
          streamSize
    messages <- ByteGet.label "messages" $ RList.byteGet Message.byteGet
    marks <- ByteGet.label "marks" $ RList.byteGet Mark.byteGet
    packages <- ByteGet.label "packages" $ RList.byteGet Str.byteGet
    objects <- ByteGet.label "objects" $ RList.byteGet ObjectName.byteGet
    names <- ByteGet.label "names" $ RList.byteGet Name.byteGet
    classMappings <-
      ByteGet.label "classMappings" $
        RList.byteGet ClassMapping.byteGet
    caches <- ByteGet.label "caches" $ RList.byteGet Cache.byteGet
    let classAttributeMap =
          ClassAttributeMap.make objects classMappings caches names
        getFrames =
          if fast
            then pure RList.empty
            else
              BitGet.toByteGet $
                Frame.decodeFramesBits
                  matchType
                  version
                  buildVersion
                  numFrames
                  maxChannels
                  classAttributeMap
    frames <- ByteGet.label "frames" $ ByteGet.embed getFrames stream
    unknown <-
      ByteGet.label "unknown" $
        fmap LazyByteString.unpack ByteGet.remaining
    pure
      Content
        { levels,
          keyframes,
          streamSize,
          frames,
          messages,
          marks,
          packages,
          objects,
          names,
          classMappings,
          caches,
          unknown
        }
