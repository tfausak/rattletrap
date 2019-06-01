module Rattletrap.Encode.Content
  ( putContent
  )
where

import Rattletrap.Encode.Cache
import Rattletrap.Encode.ClassMapping
import Rattletrap.Encode.Frame
import Rattletrap.Encode.KeyFrame
import Rattletrap.Encode.List
import Rattletrap.Encode.Mark
import Rattletrap.Encode.Message
import Rattletrap.Encode.Str
import Rattletrap.Encode.Word32le
import Rattletrap.Type.Content
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

putContent :: Content -> Binary.Put
putContent content = do
  putList putText (contentLevels content)
  putList putKeyFrame (contentKeyFrames content)
  let streamSize = contentStreamSize content
  putWord32 streamSize
  Binary.putLazyByteString
    . padLazyBytes streamSize
    . reverseLazyBytes
    . Binary.runPut
    . BinaryBits.runBitPut
    . putFrames
    $ contentFrames content
  putList putMessage (contentMessages content)
  putList putMark (contentMarks content)
  putList putText (contentPackages content)
  putList putText (contentObjects content)
  putList putText (contentNames content)
  putList putClassMapping (contentClassMappings content)
  putList putCache (contentCaches content)
  mapM_ Binary.putWord8 (contentUnknown content)

padLazyBytes :: Word32le -> LazyBytes.ByteString -> LazyBytes.ByteString
padLazyBytes size bytes = LazyBytes.take
  (fromIntegral $ word32leValue size)
  (LazyBytes.append bytes $ LazyBytes.repeat 0x00)

reverseLazyBytes :: LazyBytes.ByteString -> LazyBytes.ByteString
reverseLazyBytes = LazyBytes.map reverseByte
