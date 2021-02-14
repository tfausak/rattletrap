{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.I32 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet

newtype I32
  = I32 Int32
  deriving (Eq, Show)

$(deriveJson ''I32)

fromInt32 :: Int32 -> I32
fromInt32 = I32

toInt32 :: I32 -> Int32
toInt32 (I32 x) = x

bytePut :: I32 -> BytePut.BytePut
bytePut = BytePut.int32 . toInt32

bitPut :: I32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I32
byteGet = fromInt32 <$> ByteGet.int32

bitGet :: BitGet I32
bitGet = byteGetToBitGet byteGet 4
