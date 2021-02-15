{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.I8 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import Rattletrap.Type.Common

newtype I8
  = I8 Int8
  deriving (Eq, Show)

$(deriveJson ''I8)

fromInt8 :: Int8 -> I8
fromInt8 = I8

toInt8 :: I8 -> Int8
toInt8 (I8 x) = x

bytePut :: I8 -> BytePut.BytePut
bytePut = BytePut.int8 . toInt8

bitPut :: I8 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I8
byteGet = fromInt8 <$> ByteGet.int8

bitGet :: BitGet.BitGet I8
bitGet = BitGet.fromByteGet byteGet 1
