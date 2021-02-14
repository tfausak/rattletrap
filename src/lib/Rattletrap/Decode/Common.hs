module Rattletrap.Decode.Common
  ( BitGet
  , decodeWhen
  , getBitsLE
  , getByteStringBits
  , getWord8Bits
  , runDecodeBits
  , byteGetToBitGet
  , BinaryBits.getBool
  ) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Bits as Bits
import qualified Data.ByteString as Bytes
import qualified Data.Word as Word
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.Utility.Bytes as Utility

type BitGet = BinaryBits.BitGet

decodeWhen
  :: (Applicative m, Applicative.Alternative f) => Bool -> m a -> m (f a)
decodeWhen p f = if p then fmap pure f else pure Applicative.empty

getByteStringBits :: Int -> BitGet Bytes.ByteString
getByteStringBits = BinaryBits.getByteString

getWord8Bits :: Int -> BitGet Word.Word8
getWord8Bits = BinaryBits.getWord8

runDecodeBits :: BitGet a -> Bytes.ByteString -> Either String a
runDecodeBits = ByteGet.run . BinaryBits.runBitGet

byteGetToBitGet :: ByteGet.ByteGet a -> Int -> BitGet a
byteGetToBitGet decode size = do
  bytes <- BinaryBits.getByteString size
  case ByteGet.run decode (Utility.reverseBytes bytes) of
    Left problem -> fail problem
    Right result -> pure result

getBitsLE :: Bits.Bits a => Int -> BinaryBits.BitGet a
getBitsLE size = do
  bits <- Monad.replicateM size BinaryBits.getBool
  pure $ foldr
    (\bit x -> let y = Bits.shiftL x 1 in if bit then Bits.setBit y 0 else y)
    Bits.zeroBits
    bits
