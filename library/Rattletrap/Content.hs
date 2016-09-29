module Rattletrap.Content where

import Rattletrap.ClassMapping
import Rattletrap.KeyFrame
import Rattletrap.List
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyByteString

data Content = Content
  { contentLevels :: List Text
  , contentKeyFrames :: List KeyFrame
  , contentStreamSize :: Word32
  , contentStream :: LazyByteString.ByteString
  , contentMessages :: List Message
  , contentMarks :: List Mark
  , contentPackages :: List Text
  , contentObjects :: List Text
  , contentNames :: List Text
  , contentClassMappings :: List ClassMapping
  } deriving (Eq, Ord, Show)

getContent :: Binary.Get Content
getContent = do
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
  pure
    Content
    { contentLevels = levels
    , contentKeyFrames = keyFrames
    , contentStreamSize = streamSize
    , contentStream = stream
    , contentMessages = messages
    , contentMarks = marks
    , contentPackages = packages
    , contentObjects = objects
    , contentNames = names
    , contentClassMappings = classMappings
    }

putContent :: Content -> Binary.Put
putContent content = do
  putList putText (contentLevels content)
  putList putKeyFrame (contentKeyFrames content)
  putWord32 (contentStreamSize content)
  Binary.putLazyByteString (contentStream content)
  putList putMessage (contentMessages content)
  putList putMark (contentMarks content)
  putList putText (contentPackages content)
  putList putText (contentObjects content)
  putList putText (contentNames content)
  putList putClassMapping (contentClassMappings content)
