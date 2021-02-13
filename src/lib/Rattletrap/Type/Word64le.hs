module Rattletrap.Type.Word64le where

import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read
import Rattletrap.Encode.Common

newtype Word64le = Word64le
  { word64leValue :: Word.Word64
  } deriving (Eq, Show)

instance Aeson.FromJSON Word64le where
  parseJSON = Aeson.withText "Word64le" $
    either fail (pure . Word64le) . Read.readEither . Text.unpack

instance Aeson.ToJSON Word64le where
  toJSON = Aeson.toJSON . show . word64leValue

putWord64 :: Word64le -> Binary.Put
putWord64 word64 = Binary.putWord64le (word64leValue word64)

putWord64Bits :: Word64le -> BitPut ()
putWord64Bits word64 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putWord64 word64))
  BinaryBits.putByteString (reverseBytes bytes)

decodeWord64le :: ByteGet Word64le
decodeWord64le = Word64le <$> getWord64le

decodeWord64leBits :: BitGet Word64le
decodeWord64leBits = toBits decodeWord64le 8
