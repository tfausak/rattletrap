{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int32le where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Put as Binary

newtype Int32le
  = Int32le Int32
  deriving (Eq, Show)

$(deriveJson ''Int32le)

fromInt32 :: Int32 -> Int32le
fromInt32 = Int32le

toInt32 :: Int32le -> Int32
toInt32 (Int32le x) = x

bytePut :: Int32le -> BytePut
bytePut int32 = Binary.putInt32le (toInt32 int32)

bitPut :: Int32le -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet Int32le
byteGet = fromInt32 <$> getInt32le

bitGet :: BitGet Int32le
bitGet = toBits byteGet 4
