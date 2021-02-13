{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Int8le = Int8le
  { int8leValue :: Int8
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int8le)

putInt8 :: Int8le -> Binary.Put
putInt8 int8 = Binary.putInt8 (int8leValue int8)

putInt8Bits :: Int8le -> BinaryBits.BitPut ()
putInt8Bits int8 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putInt8 int8))
  BinaryBits.putByteString (reverseBytes bytes)

decodeInt8le :: Decode Int8le
decodeInt8le = Int8le <$> getInt8

decodeInt8leBits :: DecodeBits Int8le
decodeInt8leBits = toBits decodeInt8le 1
