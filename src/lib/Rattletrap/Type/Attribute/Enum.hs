{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Enum where

import Prelude hiding (Enum)
import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

newtype Enum = Enum
  { value :: Word16
  } deriving (Eq, Show)

$(deriveJson ''Enum)

bitPut :: Enum -> BitPut ()
bitPut enumAttribute =
  putBitsLE 11 (value enumAttribute)

bitGet :: BitGet Enum
bitGet = Enum <$> getBitsLE 11
