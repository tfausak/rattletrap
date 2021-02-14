{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Byte where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype ByteAttribute = ByteAttribute
  { value :: U8.U8
  } deriving (Eq, Show)

$(deriveJson ''ByteAttribute)

bitPut :: ByteAttribute -> BitPut ()
bitPut byteAttribute =
  U8.bitPut (value byteAttribute)

bitGet :: BitGet ByteAttribute
bitGet = ByteAttribute <$> U8.bitGet
