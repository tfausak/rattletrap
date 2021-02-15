{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.U32 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import Rattletrap.Type.Common

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

bitPut :: U32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U32
byteGet = fromWord32 <$> ByteGet.word32

bitGet :: BitGet.BitGet U32
bitGet = BitGet.fromByteGet byteGet 4
