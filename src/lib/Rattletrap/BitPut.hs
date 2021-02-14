module Rattletrap.BitPut where

import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Utility.Bytes as Utility

type BitPut = BinaryBits.BitPut ()

toBytePut :: BitPut -> BytePut.BytePut
toBytePut = Binary.execPut . BinaryBits.runBitPut

fromBytePut :: BytePut.BytePut -> BitPut
fromBytePut =
  BinaryBits.putByteString . Utility.reverseBytes . BytePut.toByteString

bits :: Bits.Bits a => Int -> a -> BitPut
bits n x = mapM_ (bool . Bits.testBit x) [0 .. n - 1]

bool :: Bool -> BitPut
bool = BinaryBits.putBool

byteString :: ByteString.ByteString -> BitPut
byteString = BinaryBits.putByteString

word8 :: Int -> Word.Word8 -> BitPut
word8 = BinaryBits.putWord8
