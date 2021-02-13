module Rattletrap.Type.Word64le where

import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read

newtype Word64le = Word64le
  { word64leValue :: Word.Word64
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Word64le where
  parseJSON value = case value of
    Aeson.String text -> case Read.readEither $ Text.unpack text of
      Left _ -> Aeson.typeMismatch "Word64le" value
      Right word64 -> pure $ Word64le word64
    Aeson.Number number -> case Scientific.toBoundedInteger number of
      Nothing -> Aeson.typeMismatch "Word64le" value
      Just word64 -> pure $ Word64le word64
    _ -> Aeson.typeMismatch "Word64le" value

instance Aeson.ToJSON Word64le where
  toJSON = Aeson.toJSON . show . word64leValue

putWord64 :: Word64le -> Binary.Put
putWord64 word64 = Binary.putWord64le (word64leValue word64)

putWord64Bits :: Word64le -> BinaryBits.BitPut ()
putWord64Bits word64 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putWord64 word64))
  BinaryBits.putByteString (reverseBytes bytes)
