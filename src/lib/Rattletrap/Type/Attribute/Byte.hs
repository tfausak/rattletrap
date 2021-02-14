{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Byte where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype ByteAttribute = ByteAttribute
  { value :: Word8le.Word8le
  } deriving (Eq, Show)

$(deriveJson ''ByteAttribute)

bitPut :: ByteAttribute -> BitPut ()
bitPut byteAttribute =
  Word8le.bitPut (value byteAttribute)

bitGet :: BitGet ByteAttribute
bitGet = ByteAttribute <$> Word8le.bitGet
