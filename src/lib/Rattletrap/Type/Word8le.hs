{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word8le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Word8le
  = Word8le Word8
  deriving (Eq, Show)

$(deriveJson ''Word8le)

fromWord8 :: Word8 -> Word8le
fromWord8 = Word8le

toWord8 :: Word8le -> Word8
toWord8 (Word8le x) = x

bytePut :: Word8le -> BytePut
bytePut = Binary.putWord8 . toWord8

bitPut :: Word8le -> BitPut ()
bitPut = BinaryBits.putByteString . reverseBytes . LazyBytes.toStrict . Binary.runPut . bytePut

byteGet :: ByteGet Word8le
byteGet = fromWord8 <$> getWord8

bitGet :: BitGet Word8le
bitGet = toBits byteGet 1
