module Rattletrap.Type.Word64le where

import Rattletrap.Decode.Common

import qualified Data.Binary.Put as Binary
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read
import Rattletrap.Encode.Common

newtype Word64le
  = Word64le Word.Word64
  deriving (Eq, Show)

instance Aeson.FromJSON Word64le where
  parseJSON = Aeson.withText "Word64le" $
    either fail (pure . fromWord64) . Read.readEither . Text.unpack

instance Aeson.ToJSON Word64le where
  toJSON = Aeson.toJSON . show . toWord64

fromWord64 :: Word.Word64 -> Word64le
fromWord64 = Word64le

toWord64 :: Word64le -> Word.Word64
toWord64 (Word64le x) = x

bytePut :: Word64le -> BytePut
bytePut = Binary.putWord64le . toWord64

bitPut :: Word64le -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet Word64le
byteGet = fromWord64 <$> getWord64le

bitGet :: BitGet Word64le
bitGet = byteGetToBitGet byteGet 8
