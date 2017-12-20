module Rattletrap.Decode.Content
  ( decodeContent
  ) where

import Rattletrap.Decode.Cache
import Rattletrap.Decode.ClassMapping
import Rattletrap.Decode.Common
import Rattletrap.Decode.Frame
import Rattletrap.Decode.KeyFrame
import Rattletrap.Decode.List
import Rattletrap.Decode.Mark
import Rattletrap.Decode.Message
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.Content
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Control.Monad.Trans.State as State

decodeContent
  :: (Int, Int, Int)
  -- ^ Version numbers, usually from 'Rattletrap.Header.getVersion'.
  -> Int
  -- ^ The number of frames in the stream, usually from
  -- 'Rattletrap.Header.getNumFrames'.
  -> Word
  -- ^ The maximum number of channels in the stream, usually from
  -- 'Rattletrap.Header.getMaxChannels'.
  -> Decode Content
decodeContent version numFrames maxChannels = do
  (levels, keyFrames, streamSize) <-
    (,,)
    <$> decodeList decodeStr
    <*> decodeList decodeKeyFrame
    <*> decodeWord32le
  (stream, messages, marks, packages, objects, names, classMappings, caches) <-
    (,,,,,,,)
    <$> getLazyByteString (fromIntegral (word32leValue streamSize))
    <*> decodeList decodeMessage
    <*> decodeList decodeMark
    <*> decodeList decodeStr
    <*> decodeList decodeStr
    <*> decodeList decodeStr
    <*> decodeList decodeClassMapping
    <*> decodeList decodeCache
  let
    classAttributeMap =
      makeClassAttributeMap objects classMappings caches names
    bitGet = State.evalStateT
      (decodeFramesBits version numFrames maxChannels classAttributeMap)
      mempty
    get = runBitGet bitGet
  frames <- either fail pure (runDecode get (reverseBytes stream))
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
