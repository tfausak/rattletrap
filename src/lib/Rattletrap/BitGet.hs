module Rattletrap.BitGet where

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Binary.Get as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Word as Word
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.Get as Get
import qualified Rattletrap.Utility.Bytes as Utility

type BitGet = BinaryBits.BitGet

toByteGet :: BitGet a -> ByteGet.ByteGet a
toByteGet = binaryGetToByteGet . BinaryBits.runBitGet

binaryGetToByteGet :: Binary.Get a -> ByteGet.ByteGet a
binaryGetToByteGet g = do
  s1 <- Get.get
  case Binary.runGetOrFail g $ LazyByteString.fromStrict s1 of
    Left (_, _, x) -> fail x
    Right (s2, _, x) -> do
      Get.put $ LazyByteString.toStrict s2
      pure x

fromByteGet :: ByteGet.ByteGet a -> Int -> BitGet a
fromByteGet f n = do
  x <- BinaryBits.getByteString n
  either fail pure . ByteGet.run f $ Utility.reverseBytes x

bits :: Bits.Bits a => Int -> BitGet a
bits n = do
  let
    f :: Bits.Bits a => Bool -> a -> a
    f bit x = let y = Bits.shiftL x 1 in if bit then Bits.setBit y 0 else y
  xs <- Monad.replicateM n bool
  pure $ foldr f Bits.zeroBits xs

bool :: BitGet Bool
bool = BinaryBits.getBool

byteString :: Int -> BitGet ByteString.ByteString
byteString = BinaryBits.getByteString

word8 :: Int -> BitGet Word.Word8
word8 = BinaryBits.getWord8
