{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word32le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Word32le = Word32le
  { word32leValue :: Word32
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word32le)

putWord32 :: Word32le -> Binary.Put
putWord32 word32 = Binary.putWord32le (word32leValue word32)

putWord32Bits :: Word32le -> BinaryBits.BitPut ()
putWord32Bits word32 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putWord32 word32))
  BinaryBits.putByteString (reverseBytes bytes)

decodeWord32le :: ByteGet Word32le
decodeWord32le = Word32le <$> getWord32le

decodeWord32leBits :: BitGet Word32le
decodeWord32leBits = toBits decodeWord32le 4
