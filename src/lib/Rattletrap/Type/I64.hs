module Rattletrap.Type.I64 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut

import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Text.Read as Read

newtype I64
  = I64 Int.Int64
  deriving (Eq, Show)

instance Aeson.FromJSON I64 where
  parseJSON =
    Aeson.withText "I64"
      $ either fail (pure . fromInt64)
      . Read.readEither
      . Text.unpack

instance Aeson.ToJSON I64 where
  toJSON = Aeson.toJSON . show . toInt64

fromInt64 :: Int.Int64 -> I64
fromInt64 = I64

toInt64 :: I64 -> Int.Int64
toInt64 (I64 x) = x

bytePut :: I64 -> BytePut.BytePut
bytePut = BytePut.int64 . toInt64

bitPut :: I64 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I64
byteGet = fromInt64 <$> ByteGet.int64

bitGet :: BitGet.BitGet I64
bitGet = BitGet.fromByteGet byteGet 8
