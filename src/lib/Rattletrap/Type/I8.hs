{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.I8 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.BitPut as BitPut

import qualified Data.Binary.Get as Binary

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

byteGet :: ByteGet I8
byteGet = fromInt8 <$> Binary.getInt8

bitGet :: BitGet I8
bitGet = byteGetToBitGet byteGet 1
