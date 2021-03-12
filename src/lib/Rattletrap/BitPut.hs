module Rattletrap.BitPut where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Rattletrap.BitBuilder as BitBuilder
import qualified Rattletrap.BytePut as BytePut

newtype BitPut = BitPut (BitBuilder.BitBuilder -> BitBuilder.BitBuilder)

instance Semigroup BitPut where
  f1 <> f2 = BitPut $ run f2 . run f1

instance Monoid BitPut where
  mempty = BitPut id

run :: BitPut -> BitBuilder.BitBuilder -> BitBuilder.BitBuilder
run (BitPut f) = f

toBytePut :: BitPut -> BytePut.BytePut
toBytePut b = BitBuilder.toBuilder $ run b BitBuilder.empty

fromBytePut :: BytePut.BytePut -> BitPut
fromBytePut = byteString . BytePut.toByteString

bits :: Bits.Bits a => Int -> a -> BitPut
bits n x = foldMap (bool . Bits.testBit x) [0 .. n - 1]

bool :: Bool -> BitPut
bool = BitPut . BitBuilder.push

byteString :: ByteString.ByteString -> BitPut
byteString = foldMap (bits 8) . ByteString.unpack
