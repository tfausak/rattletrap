module Rattletrap.Type.U64 where

import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BitGet as BitGet

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read

newtype U64
  = U64 Word.Word64
  deriving (Eq, Show)

instance Aeson.FromJSON U64 where
  parseJSON = Aeson.withText "U64" $
    either fail (pure . fromWord64) . Read.readEither . Text.unpack

instance Aeson.ToJSON U64 where
  toJSON = Aeson.toJSON . show . toWord64

fromWord64 :: Word.Word64 -> U64
fromWord64 = U64

toWord64 :: U64 -> Word.Word64
toWord64 (U64 x) = x

bytePut :: U64 -> BytePut.BytePut
bytePut = BytePut.word64 . toWord64

bitPut :: U64 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U64
byteGet = fromWord64 <$> ByteGet.word64

bitGet :: BitGet.BitGet U64
bitGet = BitGet.fromByteGet byteGet 8
