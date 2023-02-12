module Rattletrap.Type.U64 where

import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json
import qualified Text.Read as Read

newtype U64
  = U64 Word.Word64
  deriving (Eq, Show)

instance Json.FromJSON U64 where
  parseJSON =
    Json.withText "U64" $
      either fail (pure . fromWord64)
        . Read.readEither
        . Text.unpack

instance Json.ToJSON U64 where
  toJSON = Json.toJSON . show . toWord64

schema :: Schema.Schema
schema =
  Schema.named "u64" $
    Json.object [Json.pair "type" "string", Json.pair "pattern" "^[0-9]+$"]

fromWord64 :: Word.Word64 -> U64
fromWord64 = U64

toWord64 :: U64 -> Word.Word64
toWord64 (U64 x) = x

bytePut :: U64 -> BytePut.BytePut
bytePut = BytePut.word64 . toWord64

bitPut :: U64 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U64
byteGet = ByteGet.label "U64" $ fmap fromWord64 ByteGet.word64

bitGet :: BitGet.BitGet U64
bitGet = BitGet.fromByteGet byteGet 8
