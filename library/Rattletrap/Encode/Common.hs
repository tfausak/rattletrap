module Rattletrap.Encode.Common
  ( putBitsLE
  )
where

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Bits as Bits

putBitsLE :: Bits.Bits a => Int -> a -> BinaryBits.BitPut ()
putBitsLE size x = mapM_ (BinaryBits.putBool . Bits.testBit x) [0 .. size - 1]
