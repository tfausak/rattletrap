{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Enum where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

newtype EnumAttribute = EnumAttribute
  { value :: Word16
  } deriving (Eq, Show)

$(deriveJson ''EnumAttribute)

bitPut :: EnumAttribute -> BitPut ()
bitPut enumAttribute =
  putBitsLE 11 (value enumAttribute)

bitGet :: BitGet EnumAttribute
bitGet = EnumAttribute <$> getBitsLE 11
