{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word8le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Word8le = Word8le
  { word8leValue :: Word8
  } deriving (Eq, Show)

$(deriveJson ''Word8le)

putWord8 :: Word8le -> Binary.Put
putWord8 word8 = Binary.putWord8 (word8leValue word8)

putWord8Bits :: Word8le -> BinaryBits.BitPut ()
putWord8Bits word8 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putWord8 word8))
  BinaryBits.putByteString (reverseBytes bytes)

decodeWord8le :: ByteGet Word8le
decodeWord8le = Word8le <$> getWord8

decodeWord8leBits :: BitGet Word8le
decodeWord8leBits = toBits decodeWord8le 1
