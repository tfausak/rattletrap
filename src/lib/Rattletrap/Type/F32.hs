{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.F32 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet

newtype F32
  = F32 Float
  deriving (Eq, Show)

$(deriveJson ''F32)

fromFloat :: Float -> F32
fromFloat = F32

toFloat :: F32 -> Float
toFloat (F32 x) = x

bytePut :: F32 -> BytePut.BytePut
bytePut = BytePut.float . toFloat

bitPut :: F32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet F32
byteGet = fromFloat <$> ByteGet.float

bitGet :: BitGet F32
bitGet = byteGetToBitGet byteGet 4
