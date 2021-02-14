{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Float32le where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Put as Binary

newtype Float32le
  = Float32le Float
  deriving (Eq, Show)

$(deriveJson ''Float32le)

fromFloat :: Float -> Float32le
fromFloat = Float32le

toFloat :: Float32le -> Float
toFloat (Float32le x) = x

bytePut :: Float32le -> BytePut
bytePut = Binary.putFloatle . toFloat

bitPut :: Float32le -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet Float32le
byteGet = fromFloat <$> getFloatle

bitGet :: BitGet Float32le
bitGet = toBits byteGet 4
