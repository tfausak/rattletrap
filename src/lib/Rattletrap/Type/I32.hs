{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.I32 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary

newtype I32
  = I32 Int32
  deriving (Eq, Show)

$(deriveJson ''I32)

fromInt32 :: Int32 -> I32
fromInt32 = I32

toInt32 :: I32 -> Int32
toInt32 (I32 x) = x

bytePut :: I32 -> BytePut
bytePut int32 = Binary.putInt32le (toInt32 int32)

bitPut :: I32 -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet I32
byteGet = fromInt32 <$> Binary.getInt32le

bitGet :: BitGet I32
bitGet = byteGetToBitGet byteGet 4
