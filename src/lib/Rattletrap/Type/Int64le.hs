module Rattletrap.Type.Int64le where

import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Text.Read as Read

newtype Int64le = Int64le
  { int64leValue :: Int.Int64
  } deriving (Eq, Show)

instance Aeson.FromJSON Int64le where
  parseJSON = Aeson.withText "Int64le" $
    either fail (pure . Int64le) . Read.readEither . Text.unpack

instance Aeson.ToJSON Int64le where
  toJSON = Aeson.toJSON . show . int64leValue

bytePut :: Int64le -> BytePut
bytePut int64 = Binary.putInt64le (int64leValue int64)

bitPut :: Int64le -> BitPut ()
bitPut int64 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (bytePut int64))
  BinaryBits.putByteString (reverseBytes bytes)

byteGet :: ByteGet Int64le
byteGet = Int64le <$> getInt64le

bitGet :: BitGet Int64le
bitGet = toBits byteGet 8
