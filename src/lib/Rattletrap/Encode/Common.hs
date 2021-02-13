module Rattletrap.Encode.Common
  ( BytePut
  , BitPut
  , putBitsLE
  ) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Bits as Bits

type BytePut = Binary.Put

type BitPut = BinaryBits.BitPut

putBitsLE :: Bits.Bits a => Int -> a -> BitPut ()
putBitsLE size x = mapM_ (BinaryBits.putBool . Bits.testBit x) [0 .. size - 1]
