module Rattletrap.Type.Content where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Cache as Cache
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.ClassMapping as ClassMapping
import qualified Rattletrap.Type.Frame as Frame
import qualified Rattletrap.Type.Keyframe as Keyframe
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Mark as Mark
import qualified Rattletrap.Type.Message as Message
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Bytes as Bytes
import qualified Rattletrap.Vendor.Argo as Argo

type Content = ContentWith (List.List Frame.Frame)

-- | Contains low-level game data about a 'Rattletrap.Replay.Replay'.
data ContentWith frames = Content
  { levels :: List.List Str.Str
  -- ^ This typically only has one element, like @stadium_oob_audio_map@.
  , keyframes :: List.List Keyframe.Keyframe
  -- ^ A list of which frames are key frames. Although they aren't necessary
  -- for replay, key frames are frames that replicate every actor. They
  -- typically happen once every 10 seconds.
  , streamSize :: U32.U32
  -- ^ The size of the stream in bytes. This is only really necessary because
  -- the stream has some arbitrary amount of padding at the end.
  , frames :: frames
  -- ^ The actual game data. This is where all the interesting information is.
  , messages :: List.List Message.Message
  -- ^ Debugging messages. In newer replays, this is always empty.
  , marks :: List.List Mark.Mark
  -- ^ Tick marks shown on the scrubber when watching a replay.
  , packages :: List.List Str.Str
  -- ^ A list of @.upk@ files to load, like
  -- @..\\..\\TAGame\\CookedPCConsole\\Stadium_P.upk@.
  , objects :: List.List Str.Str
  -- ^ Objects in the stream. Used for the
  -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , names :: List.List Str.Str
  -- ^ It's not clear what these are used for. This list is usually not empty,
  -- but appears unused otherwise.
  , classMappings :: List.List ClassMapping.ClassMapping
  -- ^ A mapping between classes and their ID in the stream. Used for the
  -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , caches :: List.List Cache.Cache
  -- ^ A list of classes along with their parent classes and attributes. Used
  -- for the 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , unknown :: [Word.Word8]
  }
  deriving (Eq, Show)

instance Argo.HasCodec f => Argo.HasCodec (ContentWith f) where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Content
      <$> Argo.required levels "levels"
      <*> Argo.required keyframes "key_frames"
      <*> Argo.required streamSize "stream_size"
      <*> Argo.required frames "frames"
      <*> Argo.required messages "messages"
      <*> Argo.required marks "marks"
      <*> Argo.required packages "packages"
      <*> Argo.required objects "objects"
      <*> Argo.required names "names"
      <*> Argo.required classMappings "class_mappings"
      <*> Argo.required caches "caches"
      <*> Argo.required unknown "unknown"

empty :: Content
empty = Content
  { levels = List.empty
  , keyframes = List.empty
  , streamSize = U32.fromWord32 0
  , frames = List.empty
  , messages = List.empty
  , marks = List.empty
  , packages = List.empty
  , objects = List.empty
  , names = List.empty
  , classMappings = List.empty
  , caches = List.empty
  , unknown = []
  }

bytePut :: Content -> BytePut.BytePut
bytePut x =
  List.bytePut Str.bytePut (levels x)
    <> List.bytePut Keyframe.bytePut (keyframes x)
    <> putFrames x
    <> List.bytePut Message.bytePut (messages x)
    <> List.bytePut Mark.bytePut (marks x)
    <> List.bytePut Str.bytePut (packages x)
    <> List.bytePut Str.bytePut (objects x)
    <> List.bytePut Str.bytePut (names x)
    <> List.bytePut ClassMapping.bytePut (classMappings x)
    <> List.bytePut Cache.bytePut (caches x)
    <> foldMap BytePut.word8 (unknown x)

putFrames :: Content -> BytePut.BytePut
putFrames x =
  let
    stream =
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
    streamSize_ = U32.fromWord32
      $ max (U32.toWord32 expectedStreamSize) (U32.toWord32 actualStreamSize)
  in U32.bytePut streamSize_
    <> BytePut.byteString (Bytes.padBytes (U32.toWord32 streamSize_) stream)

byteGet
  :: Maybe Str.Str
  -> Version.Version
  -- ^ Version numbers, usually from 'Rattletrap.Header.getVersion'.
  -> Int
  -- ^ The number of frames in the stream, usually from
  -- 'Rattletrap.Header.getNumFrames'.
  -> Word
  -- ^ The maximum number of channels in the stream, usually from
  -- 'Rattletrap.Header.getMaxChannels'.
  -> ByteGet.ByteGet Content
byteGet matchType version numFrames maxChannels = ByteGet.label "Content" $ do
  levels <- ByteGet.label "levels" $ List.byteGet Str.byteGet
  keyframes <- ByteGet.label "keyframes" $ List.byteGet Keyframe.byteGet
  streamSize <- ByteGet.label "streamSize" U32.byteGet
  stream <-
    ByteGet.label "stream" . ByteGet.byteString . fromIntegral $ U32.toWord32
      streamSize
  messages <- ByteGet.label "messages" $ List.byteGet Message.byteGet
  marks <- ByteGet.label "marks" $ List.byteGet Mark.byteGet
  packages <- ByteGet.label "packages" $ List.byteGet Str.byteGet
  objects <- ByteGet.label "objects" $ List.byteGet Str.byteGet
  names <- ByteGet.label "names" $ List.byteGet Str.byteGet
  classMappings <- ByteGet.label "classMappings"
    $ List.byteGet ClassMapping.byteGet
  caches <- ByteGet.label "caches" $ List.byteGet Cache.byteGet
  let
    classAttributeMap =
      ClassAttributeMap.make objects classMappings caches names
    getFrames = BitGet.toByteGet $ Frame.decodeFramesBits
      matchType
      version
      numFrames
      maxChannels
      classAttributeMap
  frames <- ByteGet.label "frames" $ ByteGet.embed getFrames stream
  unknown <- ByteGet.label "unknown"
    $ fmap LazyByteString.unpack ByteGet.remaining
  pure Content
    { levels
    , keyframes
    , streamSize
    , frames
    , messages
    , marks
    , packages
    , objects
    , names
    , classMappings
    , caches
    , unknown
    }
