{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Int8le
  = Int8le Int8
  deriving (Eq, Show)

$(deriveJson ''Int8le)

fromInt8 :: Int8 -> Int8le
fromInt8 = Int8le

toInt8 :: Int8le -> Int8
toInt8 (Int8le x) = x

bytePut :: Int8le -> BytePut
bytePut int8 = Binary.putInt8 (toInt8 int8)

bitPut :: Int8le -> BitPut ()
bitPut int8 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (bytePut int8))
  BinaryBits.putByteString (reverseBytes bytes)

byteGet :: ByteGet Int8le
byteGet = fromInt8 <$> getInt8

bitGet :: BitGet Int8le
bitGet = toBits byteGet 1
