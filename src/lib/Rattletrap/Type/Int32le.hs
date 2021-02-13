{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int32le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Int32le = Int32le
  { int32leValue :: Int32
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int32le)

putInt32 :: Int32le -> Binary.Put
putInt32 int32 = Binary.putInt32le (int32leValue int32)

putInt32Bits :: Int32le -> BinaryBits.BitPut ()
putInt32Bits int32 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (putInt32 int32))
  BinaryBits.putByteString (reverseBytes bytes)
