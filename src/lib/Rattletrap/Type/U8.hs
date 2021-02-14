{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.U8 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary as Binary

newtype U8
  = U8 Word8
  deriving (Eq, Show)

$(deriveJson ''U8)

fromWord8 :: Word8 -> U8
fromWord8 = U8

toWord8 :: U8 -> Word8
toWord8 (U8 x) = x

bytePut :: U8 -> BytePut
bytePut = Binary.putWord8 . toWord8

bitPut :: U8 -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet U8
byteGet = fromWord8 <$> Binary.getWord8

bitGet :: BitGet U8
bitGet = byteGetToBitGet byteGet 1
