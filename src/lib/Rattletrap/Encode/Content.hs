module Rattletrap.Encode.Content
  ( putContent
  ) where

import Rattletrap.Encode.Cache
import Rattletrap.Encode.ClassMapping
import Rattletrap.Encode.Frame
import Rattletrap.Type.KeyFrame
import Rattletrap.Type.List
import Rattletrap.Type.Mark
import Rattletrap.Type.Message
import Rattletrap.Type.Str
import Rattletrap.Type.Content
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

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
