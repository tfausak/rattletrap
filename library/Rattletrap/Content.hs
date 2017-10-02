module Rattletrap.Content where

import Debug.Trace
import Data.Function
import Text.Printf
import Rattletrap.AttributeMapping
import Rattletrap.Cache
import Rattletrap.ClassAttributeMap
import Rattletrap.ClassMapping
import Rattletrap.Frame
import Rattletrap.KeyFrame
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Primitive
import Rattletrap.Utility

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Map as Map

-- | Contains low-level game data about a 'Rattletrap.Replay.Replay'.
data Content = Content
  { contentLevels :: List Text
  -- ^ This typically only has one element, like @stadium_oob_audio_map@.
  , contentKeyFrames :: List KeyFrame
  -- ^ A list of which frames are key frames. Although they aren't necessary
  -- for replay, key frames are frames that replicate every actor. They
  -- typically happen once every 10 seconds.
  , contentStreamSize :: Word32
  -- ^ The size of the stream in bytes. This is only really necessary because
  -- the stream has some arbitrary amount of padding at the end.
  , contentFrames :: [Frame]
  -- ^ The actual game data. This is where all the interesting information is.
  , contentMessages :: List Message
  -- ^ Debugging messages. In newer replays, this is always empty.
  , contentMarks :: List Mark
  -- ^ Tick marks shown on the scrubber when watching a replay.
  , contentPackages :: List Text
  -- ^ A list of @.upk@ files to load, like
  -- @..\\..\\TAGame\\CookedPCConsole\\Stadium_P.upk@.
  , contentObjects :: List Text
  -- ^ Objects in the stream. Used for the
  -- 'Rattletrap.ClassAttributeMap.ClassAttributeMap'.
  , contentNames :: List Text
  -- ^ It's not clear what these are used for. This list is usually not empty,
  -- but appears unused otherwise.
  , contentClassMappings :: List ClassMapping
  -- ^ A mapping between classes and their ID in the stream. Used for the
  -- 'Rattletrap.ClassAttributeMap.ClassAttributeMap'.
  , contentCaches :: List Cache
  -- ^ A list of classes along with their parent classes and attributes. Used
  -- for the 'Rattletrap.ClassAttributeMap.ClassAttributeMap'.
  } deriving (Eq, Ord, Show)

getContent
  :: (Int, Int)
  -- ^ Major and minor version numbers, usually from
  -- 'Rattletrap.Header.getVersion'.
  -> Int
  -- ^ The number of frames in the stream, usually from
  -- 'Rattletrap.Header.getNumFrames'.
  -> Word
  -- ^ The maximum number of channels in the stream, usually from
  -- 'Rattletrap.Header.getMaxChannels'.
  -> Binary.Get Content
getContent version numFrames maxChannels = do
  levels <- getList getText
  levels
    & listValue
    & map (\ x -> printf "  - %s" (textToString x))
    & ("Content levels:" :)
    & unlines
    & traceM
  keyFrames <- getList getKeyFrame
  keyFrames
    & listValue
    & map (\ x -> printf "  - Time: %f\n    Frame: %d\n    Position: %d"
      (x & keyFrameTime & float32Value)
      (x & keyFrameFrame & word32Value)
      (x & keyFramePosition & word32Value))
    & ("Content key frames:" :)
    & unlines
    & traceM
  streamSize <- getWord32
  traceM (printf "Content stream size: %d\n" (word32Value streamSize))
  stream <- Binary.getLazyByteString (fromIntegral (word32Value streamSize))
  messages <- getList getMessage
  messages
    & listValue
    & map (\ x -> printf "  - %s" (show x))
    & ("Content messages:" :)
    & unlines
    & traceM
  marks <- getList getMark
  marks
    & listValue
    & map (\ x -> printf "  - Value: %s\n    Frame: %d"
      (x & markValue & textToString)
      (x & markFrame & word32Value))
    & ("Content marks:" :)
    & unlines
    & traceM
  packages <- getList getText
  packages
    & listValue
    & map (\ x -> printf "  - %s" (textToString x))
    & ("Content packages:" :)
    & unlines
    & traceM
  objects <- getList getText
  objects
    & listValue
    & zip [0 ..]
    & map (\ (i, x) -> printf "  %d: %s" (i :: Int) (textToString x))
    & ("Content objects:" :)
    & unlines
    & traceM
  names <- getList getText
  names
    & listValue
    & zip [0 ..]
    & map (\ (i, x) -> printf "  %d: %s" (i :: Int) (textToString x))
    & ("Content names:" :)
    & unlines
    & traceM
  classMappings <- getList getClassMapping
  classMappings
    & listValue
    & map (\ x -> printf "  %s: %d"
      (x & classMappingName & textToString)
      (x & classMappingStreamId & word32Value))
    & ("Content class mappings:" :)
    & unlines
    & traceM
  caches <- getList getCache
  caches
    & listValue
    & map (\ x -> printf "  - Class ID: %d\n    Parent cache ID: %d\n    Cache ID: %d\n    Attribute map:\n%s"
      (x & cacheClassId & word32Value)
      (x & cacheParentCacheId & word32Value)
      (x & cacheCacheId & word32Value)
      (x
        & cacheAttributeMappings
        & listValue
        & map (\ y -> printf "      %d: %d"
          (y & attributeMappingObjectId & word32Value)
          (y & attributeMappingStreamId & word32Value))
        & unlines))
    & ("Content caches:" :)
    & unlines
    & traceM
  let classAttributeMap =
        makeClassAttributeMap objects classMappings caches names
  let frames =
        Binary.runGet
          (BinaryBit.runBitGet
             (do (theFrames, _) <-
                   getFrames
                     version
                     (0, numFrames)
                     maxChannels
                     classAttributeMap
                     Map.empty
                 pure theFrames))
          (reverseBytes stream)
  pure
    (Content
       levels
       keyFrames
       streamSize
       frames
       messages
       marks
       packages
       objects
       names
       classMappings
       caches)

putContent :: Content -> Binary.Put
putContent content = do
  putList putText (contentLevels content)
  putList putKeyFrame (contentKeyFrames content)
  let streamSize = contentStreamSize content
  putWord32 streamSize
  let stream =
        Binary.runPut (BinaryBit.runBitPut (putFrames (contentFrames content)))
  Binary.putLazyByteString
    (reverseBytes (padBytes (word32Value streamSize) stream))
  putList putMessage (contentMessages content)
  putList putMark (contentMarks content)
  putList putText (contentPackages content)
  putList putText (contentObjects content)
  putList putText (contentNames content)
  putList putClassMapping (contentClassMappings content)
  putList putCache (contentCaches content)
