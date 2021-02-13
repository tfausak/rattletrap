{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int32le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Int32le = Int32le
  { int32leValue :: Int32
  } deriving (Eq, Show)

$(deriveJson ''Int32le)

putInt32 :: Int32le -> BytePut
putInt32 int32 = Binary.putInt32le (int32leValue int32)

putInt32Bits :: Int32le -> BitPut ()
putInt32Bits int32 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putInt32 int32))
  BinaryBits.putByteString (reverseBytes bytes)

decodeInt32le :: ByteGet Int32le
decodeInt32le = Int32le <$> getInt32le

decodeInt32leBits :: BitGet Int32le
decodeInt32leBits = toBits decodeInt32le 4
