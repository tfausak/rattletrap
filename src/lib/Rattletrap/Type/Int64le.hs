module Rattletrap.Type.Int64le where

import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Put as Binary
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Text.Read as Read

newtype Int64le
  = Int64le Int.Int64
  deriving (Eq, Show)

instance Aeson.FromJSON Int64le where
  parseJSON = Aeson.withText "Int64le" $
    either fail (pure . fromInt64) . Read.readEither . Text.unpack

instance Aeson.ToJSON Int64le where
  toJSON = Aeson.toJSON . show . toInt64

fromInt64 :: Int.Int64 -> Int64le
fromInt64 = Int64le

toInt64 :: Int64le -> Int.Int64
toInt64 (Int64le x) = x

bytePut :: Int64le -> BytePut
bytePut int64 = Binary.putInt64le (toInt64 int64)

bitPut :: Int64le -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet Int64le
byteGet = fromInt64 <$> getInt64le

bitGet :: BitGet Int64le
bitGet = toBits byteGet 8
