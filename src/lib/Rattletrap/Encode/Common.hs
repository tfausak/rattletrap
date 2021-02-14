module Rattletrap.Encode.Common where

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Rattletrap.Utility.Bytes as Utility

type BytePut = Binary.Put

type BitPut = BinaryBits.BitPut

putBitsLE :: Bits.Bits a => Int -> a -> BitPut ()
putBitsLE size x = mapM_ (BinaryBits.putBool . Bits.testBit x) [0 .. size - 1]

bytePutToBitPut :: (a -> BytePut) -> a -> BitPut ()
bytePutToBitPut f =
  BinaryBits.putByteString
  . Utility.reverseBytes
  . LazyBytes.toStrict
  . Binary.runPut
  . f
