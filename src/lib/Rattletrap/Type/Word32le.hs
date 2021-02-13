{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word32le where

import Rattletrap.Type.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

newtype Word32le
  = Word32le Word32
  deriving (Eq, Ord, Show)

$(deriveJson ''Word32le)

fromWord32 :: Word32 -> Word32le
fromWord32 = Word32le

toWord32 :: Word32le -> Word32
toWord32 (Word32le x) = x

bytePut :: Word32le -> BytePut
bytePut = Binary.putWord32le . toWord32

bitPut :: Word32le -> BitPut ()
bitPut = BinaryBits.putByteString . reverseBytes . LazyBytes.toStrict . Binary.runPut . bytePut

byteGet :: ByteGet Word32le
byteGet = fromWord32 <$> getWord32le

bitGet :: BitGet Word32le
bitGet = toBits byteGet 4
