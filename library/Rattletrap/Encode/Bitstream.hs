module Rattletrap.Encode.Bitstream
  ( putBitstream
  )
where

import Rattletrap.Type.Bitstream

import qualified Data.Binary.Bits.Put as BinaryBits

putBitstream :: Bitstream -> BinaryBits.BitPut ()
putBitstream = mapM_ BinaryBits.putBool . bitstreamValue
