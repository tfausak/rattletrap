{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Content where

import Rattletrap.Type.Cache
import Rattletrap.Type.ClassMapping
import Rattletrap.Type.Common
import Rattletrap.Type.Frame
import Rattletrap.Type.KeyFrame
import Rattletrap.Type.List
import Rattletrap.Type.Mark
import Rattletrap.Type.Message
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

-- | Contains low-level game data about a 'Rattletrap.Replay.Replay'.
data Content = Content
  { contentLevels :: List Str
  -- ^ This typically only has one element, like @stadium_oob_audio_map@.
  , contentKeyFrames :: List KeyFrame
  -- ^ A list of which frames are key frames. Although they aren't necessary
  -- for replay, key frames are frames that replicate every actor. They
  -- typically happen once every 10 seconds.
  , contentStreamSize :: Word32le
  -- ^ The size of the stream in bytes. This is only really necessary because
  -- the stream has some arbitrary amount of padding at the end.
  , contentFrames :: [Frame]
  -- ^ The actual game data. This is where all the interesting information is.
  , contentMessages :: List Message
  -- ^ Debugging messages. In newer replays, this is always empty.
  , contentMarks :: List Mark
  -- ^ Tick marks shown on the scrubber when watching a replay.
  , contentPackages :: List Str
  -- ^ A list of @.upk@ files to load, like
  -- @..\\..\\TAGame\\CookedPCConsole\\Stadium_P.upk@.
  , contentObjects :: List Str
  -- ^ Objects in the stream. Used for the
  -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , contentNames :: List Str
  -- ^ It's not clear what these are used for. This list is usually not empty,
  -- but appears unused otherwise.
  , contentClassMappings :: List ClassMapping
  -- ^ A mapping between classes and their ID in the stream. Used for the
  -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , contentCaches :: List Cache
  -- ^ A list of classes along with their parent classes and attributes. Used
  -- for the 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , contentUnknown :: [Word8]
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Content)

defaultContent :: Content
defaultContent = Content
  { contentLevels = List []
  , contentKeyFrames = List []
  , contentStreamSize = Word32le 0
  , contentFrames = []
  , contentMessages = List []
  , contentMarks = List []
  , contentPackages = List []
  , contentObjects = List []
  , contentNames = List []
  , contentClassMappings = List []
  , contentCaches = List []
  , contentUnknown = []
  }

putContent :: Content -> Binary.Put
putContent content = do
  putList putText (contentLevels content)
  putList putKeyFrame (contentKeyFrames content)
  let
    stream = LazyBytes.toStrict
      (Binary.runPut (BinaryBits.runBitPut (putFrames (contentFrames content)))
      )
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
    expectedStreamSize = contentStreamSize content
    actualStreamSize = Word32le . fromIntegral $ Bytes.length stream
    streamSize = Word32le $ max
      (word32leValue expectedStreamSize)
      (word32leValue actualStreamSize)
  putWord32 streamSize
  Binary.putByteString
    (reverseBytes (padBytes (word32leValue streamSize) stream))
  putList putMessage (contentMessages content)
  putList putMark (contentMarks content)
  putList putText (contentPackages content)
  putList putText (contentObjects content)
  putList putText (contentNames content)
  putList putClassMapping (contentClassMappings content)
  putList putCache (contentCaches content)
  mapM_ Binary.putWord8 (contentUnknown content)
