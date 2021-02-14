{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8le where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Put as Binary

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
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet Int8le
byteGet = fromInt8 <$> getInt8

bitGet :: BitGet Int8le
bitGet = toBits byteGet 1
