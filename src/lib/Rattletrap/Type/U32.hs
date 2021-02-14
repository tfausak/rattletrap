{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.U32 where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common
import qualified Rattletrap.BytePut as BytePut

import qualified Data.Binary.Get as Binary

newtype U32
  = U32 Word32
  deriving (Eq, Ord, Show)

$(deriveJson ''U32)

fromWord32 :: Word32 -> U32
fromWord32 = U32

toWord32 :: U32 -> Word32
toWord32 (U32 x) = x

bytePut :: U32 -> BytePut.BytePut
bytePut = BytePut.word32 . toWord32

bitPut :: U32 -> BitPut ()
bitPut = bytePutToBitPut bytePut

byteGet :: ByteGet U32
byteGet = fromWord32 <$> Binary.getWord32le

bitGet :: BitGet U32
bitGet = byteGetToBitGet byteGet 4
