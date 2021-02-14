module Rattletrap.BitGet where

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.Utility.Bytes as Utility

type BitGet = BinaryBits.BitGet

toByteGet :: BitGet a -> ByteGet.ByteGet a
toByteGet = BinaryBits.runBitGet

fromByteGet :: ByteGet.ByteGet a -> Int -> BitGet a
fromByteGet f n = do
  x <- BinaryBits.getByteString n
  either fail pure . ByteGet.run f $ Utility.reverseBytes x

bits :: Bits.Bits a => Int -> BitGet a
bits n =
  foldr
    (\bit x -> let y = Bits.shiftL x 1 in if bit then Bits.setBit y 0 else y)
    Bits.zeroBits <$> Monad.replicateM n bool

bool :: BitGet Bool
bool = BinaryBits.getBool

byteString :: Int -> BitGet ByteString.ByteString
byteString = BinaryBits.getByteString

word8 :: Int -> BitGet Word.Word8
word8 = BinaryBits.getWord8
