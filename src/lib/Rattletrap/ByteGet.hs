module Rattletrap.ByteGet where

import qualified Control.Exception as Exception
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified GHC.Float as Float
import qualified Rattletrap.Exception.NotEnoughInput as NotEnoughInput
import qualified Rattletrap.Get as Get

type ByteGet = Get.Get ByteString.ByteString Identity.Identity

run :: ByteGet a -> ByteString.ByteString -> Either Exception.SomeException a
run g = fmap snd . Identity.runIdentity . Get.run g

byteString :: Int -> ByteGet ByteString.ByteString
byteString n = do
  s1 <- Get.get
  let (x, s2) = ByteString.splitAt n s1
  if ByteString.length x == n
    then do
      Get.put s2
      pure x
    else throw NotEnoughInput.NotEnoughInput

float :: ByteGet Float
float = fmap Float.castWord32ToFloat word32

int8 :: ByteGet Int.Int8
int8 = fmap fromIntegral word8

int32 :: ByteGet Int.Int32
int32 = fmap fromIntegral word32

int64 :: ByteGet Int.Int64
int64 = fmap fromIntegral word64

remaining :: ByteGet LazyByteString.ByteString
remaining = do
  x <- Get.get
  Get.put ByteString.empty
  pure $ LazyByteString.fromStrict x

word8 :: ByteGet Word.Word8
word8 = fmap ByteString.head $ byteString 1

word32 :: ByteGet Word.Word32
word32 = do
  x <- byteString 4
  pure
    $ Bits.shiftL (fromIntegral $ ByteString.index x 0) 0
    + Bits.shiftL (fromIntegral $ ByteString.index x 1) 8
    + Bits.shiftL (fromIntegral $ ByteString.index x 2) 16
    + Bits.shiftL (fromIntegral $ ByteString.index x 3) 24

word64 :: ByteGet Word.Word64
word64 = do
  x <- byteString 8
  pure
    $ Bits.shiftL (fromIntegral $ ByteString.index x 0) 0
    + Bits.shiftL (fromIntegral $ ByteString.index x 1) 8
    + Bits.shiftL (fromIntegral $ ByteString.index x 2) 16
    + Bits.shiftL (fromIntegral $ ByteString.index x 3) 24
    + Bits.shiftL (fromIntegral $ ByteString.index x 4) 32
    + Bits.shiftL (fromIntegral $ ByteString.index x 5) 40
    + Bits.shiftL (fromIntegral $ ByteString.index x 6) 48
    + Bits.shiftL (fromIntegral $ ByteString.index x 7) 56

throw :: Exception.Exception e => e -> ByteGet a
throw = Get.throw

embed :: ByteGet a -> ByteString.ByteString -> ByteGet a
embed = Get.embed
