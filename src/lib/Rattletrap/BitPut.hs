{- hlint ignore "Avoid restricted extensions" -}
{-# LANGUAGE FlexibleInstances #-}

module Rattletrap.BitPut where

import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Utility.Bytes as Utility

type BitPut = BitPutM ()

newtype BitPutM a = BitPutM (BinaryBits.BitPut a)

instance Functor BitPutM where
  fmap f = fromBinaryBits . fmap f . toBinaryBits

instance Applicative BitPutM where
  pure = fromBinaryBits . pure

  x <*> y = fromBinaryBits $ toBinaryBits x <*> toBinaryBits y

-- instance Monad BitPutM where
--   x >>= f = fromBinaryBits $ toBinaryBits x >>= toBinaryBits . f

instance Semigroup (BitPutM a) where
  x <> y = fromBinaryBits $ toBinaryBits x *> toBinaryBits y

instance Monoid (BitPutM ()) where
  mempty = fromBinaryBits $ pure ()

fromBinaryBits :: BinaryBits.BitPut a -> BitPutM a
fromBinaryBits = BitPutM

toBinaryBits :: BitPutM a -> BinaryBits.BitPut a
toBinaryBits (BitPutM x) = x

toBytePut :: BitPut -> BytePut.BytePut
toBytePut = Binary.execPut . BinaryBits.runBitPut . toBinaryBits

fromBytePut :: BytePut.BytePut -> BitPut
fromBytePut =
  fromBinaryBits
  . BinaryBits.putByteString
  . Utility.reverseBytes
  . BytePut.toByteString

bits :: Bits.Bits a => Int -> a -> BitPut
bits n x = foldMap (bool . Bits.testBit x) [0 .. n - 1]

bool :: Bool -> BitPut
bool = fromBinaryBits . BinaryBits.putBool

byteString :: ByteString.ByteString -> BitPut
byteString = fromBinaryBits . BinaryBits.putByteString

word8 :: Int -> Word.Word8 -> BitPut
word8 n = fromBinaryBits . BinaryBits.putWord8 n
