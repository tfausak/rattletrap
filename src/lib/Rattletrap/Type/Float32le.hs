{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Float32le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

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
bitPut float32 = do
  let bytes = LazyBytes.toStrict (Binary.runPut (bytePut float32))
  BinaryBits.putByteString (reverseBytes bytes)

byteGet :: ByteGet Float32le
byteGet = fromFloat <$> getFloatle

bitGet :: BitGet Float32le
bitGet = toBits byteGet 4
