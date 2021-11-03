module Rattletrap.Type.U32 where

import qualified Argo as Argo
import qualified Argo.Codec
import qualified Argo.Json.Number
import qualified Control.Applicative as Applicative
import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype U32
  = U32 Word.Word32
  deriving (Eq, Ord, Show)

instance Json.FromValue U32 where
  fromValue = fmap fromWord32 . Json.fromValue

instance Json.ToValue U32 where
  toValue = Json.toValue . toWord32

codec :: Argo.Codec.ValueCodec U32
codec = Argo.Codec.dimap fromWord32 toWord32 word32Codec

word32Codec :: Argo.Codec.ValueCodec Word.Word32
word32Codec = Argo.Codec.Codec
  { Argo.Codec.decode = do
    Argo.Json.Number.Number s e <- Argo.Codec.decode Argo.Codec.numberCodec
    if e < 0
      then Applicative.empty
      else pure . fromIntegral $ s * 10 ^ e
  , Argo.Codec.encode = \ x -> do
    _ <- Argo.Codec.encode Argo.Codec.numberCodec $ Argo.Json.Number.number (fromIntegral x) 0
    pure x
  }

schema :: Schema.Schema
schema = Schema.named "u32" $ Json.object
  [ Json.pair "type" "integer"
  , Json.pair "minimum" (minBound :: Word.Word32)
  , Json.pair "maximum" (maxBound :: Word.Word32)
  ]

fromWord32 :: Word.Word32 -> U32
fromWord32 = U32

toWord32 :: U32 -> Word.Word32
toWord32 (U32 x) = x

bytePut :: U32 -> BytePut.BytePut
bytePut = BytePut.word32 . toWord32

bitPut :: U32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U32
byteGet = ByteGet.label "U32" $ fmap fromWord32 ByteGet.word32

bitGet :: BitGet.BitGet U32
bitGet = BitGet.fromByteGet byteGet 4
