module Rattletrap.Decode.Content
  ( getContent
  ) where

import Rattletrap.Type.Content
import Rattletrap.Decode.Cache
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Decode.ClassMapping
import Rattletrap.Decode.Frame
import Rattletrap.Decode.KeyFrame
import Rattletrap.Decode.Mark
import Rattletrap.Decode.Message
import Rattletrap.Decode.List
import Rattletrap.Decode.Text
import Rattletrap.Type.Word32
import Rattletrap.Decode.Word32
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Map as Map

getContent
  :: (Int, Int, Int)
  -- ^ Version numbers, usually from 'Rattletrap.Header.getVersion'.
  -> Int
  -- ^ The number of frames in the stream, usually from
  -- 'Rattletrap.Header.getNumFrames'.
  -> Word
  -- ^ The maximum number of channels in the stream, usually from
  -- 'Rattletrap.Header.getMaxChannels'.
  -> Binary.Get Content
getContent version numFrames maxChannels = do
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
  let
    classAttributeMap =
      makeClassAttributeMap objects classMappings caches names
  let
    frames = Binary.runGet
      ( BinaryBit.runBitGet
        ( do
          (theFrames, _) <- getFrames
            version
            numFrames
            maxChannels
            classAttributeMap
            Map.empty
          pure theFrames
        )
      )
      (reverseBytes stream)
  pure
    ( Content
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
      caches
    )
