module Rattletrap.Content where

import Rattletrap.ActorMap
import Rattletrap.Cache
import Rattletrap.ClassAttributeMap
import Rattletrap.ClassMapping
import Rattletrap.Frame
import Rattletrap.KeyFrame
import Rattletrap.List
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Text
import Rattletrap.Utility
import Rattletrap.Word32

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary

data Content = Content
  { contentLevels :: List Text
  , contentKeyFrames :: List KeyFrame
  , contentStreamSize :: Word32
  , contentFrames :: [Frame]
  , contentMessages :: List Message
  , contentMarks :: List Mark
  , contentPackages :: List Text
  , contentObjects :: List Text
  , contentNames :: List Text
  , contentClassMappings :: List ClassMapping
  , contentCaches :: List Cache
  } deriving (Eq, Ord, Show)

getContent :: Int -> Binary.Get Content
getContent numFrames = do
  levels <- getList getText
  keyFrames <- getList getKeyFrame
  streamSize <- getWord32
  stream <- Binary.getLazyByteString (fromIntegral (word32Value streamSize))
  messages <- getList getMessage
  marks <- getList getMark
  packages <- getList getText
  objects <- getList getText
  names <- getList getText
  classMappings <- getList getClassMapping
  caches <- getList getCache
  let classAttributeMap = makeClassAttributeMap objects classMappings caches
  let (frames, _) =
        Binary.runGet
          (BinaryBit.runBitGet
             (getFrames numFrames classAttributeMap makeActorMap))
          (reverseBytes stream)
  pure
    Content
    { contentLevels = levels
    , contentKeyFrames = keyFrames
    , contentStreamSize = streamSize
    , contentFrames = frames
    , contentMessages = messages
    , contentMarks = marks
    , contentPackages = packages
    , contentObjects = objects
    , contentNames = names
    , contentClassMappings = classMappings
    , contentCaches = caches
    }

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
