{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.F32 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common
import qualified Rattletrap.BytePut as BytePut

import qualified Data.Binary.Get as Binary

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

bitPut :: F32 -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet F32
byteGet = fromFloat <$> Binary.getFloatle

bitGet :: BitGet F32
bitGet = byteGetToBitGet byteGet 4
