{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word32le where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Put as Binary

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
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet Word32le
byteGet = fromWord32 <$> getWord32le

bitGet :: BitGet Word32le
bitGet = byteGetToBitGet byteGet 4
