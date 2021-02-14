module Rattletrap.Type.U64 where

import Rattletrap.Decode.Common

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read
import Rattletrap.Encode.Common

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

bytePut :: U64 -> BytePut
bytePut = Binary.putWord64le . toWord64

bitPut :: U64 -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet U64
byteGet = fromWord64 <$> Binary.getWord64le

bitGet :: BitGet U64
bitGet = byteGetToBitGet byteGet 8
