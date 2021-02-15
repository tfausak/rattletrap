module Rattletrap.BitPut where

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Utility.Bytes as Utility

newtype BitPut = BitPut (BinaryBits.BitPut ())

instance Semigroup BitPut where
  x <> y = fromBinaryBits $ toBinaryBits x >> toBinaryBits y

instance Monoid BitPut where
  mempty = fromBinaryBits $ pure ()

fromBinaryBits :: BinaryBits.BitPut () -> BitPut
fromBinaryBits = BitPut

toBinaryBits :: BitPut -> BinaryBits.BitPut ()
toBinaryBits (BitPut x) = x

toBytePut :: BitPut -> BytePut.BytePut
toBytePut = Binary.execPut . BinaryBits.runBitPut . toBinaryBits

fromBytePut :: BytePut.BytePut -> BitPut
fromBytePut = byteString . Utility.reverseBytes . BytePut.toByteString

bits :: Bits.Bits a => Int -> a -> BitPut
bits n x = foldMap (bool . Bits.testBit x) [0 .. n - 1]

bool :: Bool -> BitPut
bool = fromBinaryBits . BinaryBits.putBool

byteString :: ByteString.ByteString -> BitPut
byteString = fromBinaryBits . BinaryBits.putByteString

word8 :: Int -> Word.Word8 -> BitPut
word8 n = fromBinaryBits . BinaryBits.putWord8 n
