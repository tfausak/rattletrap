module Rattletrap.BitString where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString

data BitString = BitString
  { byteString :: ByteString.ByteString,
    offset :: Int
  }
  deriving (Eq, Show)

fromByteString :: ByteString.ByteString -> BitString
fromByteString byteString = BitString {byteString, offset = 0}

pop :: BitString -> Maybe (Bool, BitString)
pop old = do
  (word, byteString) <- ByteString.uncons $ byteString old
  let bit = Bits.testBit word $ offset old
      new =
        if offset old == 7
          then fromByteString byteString
          else old {offset = offset old + 1}
  pure (bit, new)
