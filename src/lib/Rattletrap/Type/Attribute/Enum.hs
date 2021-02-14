{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Enum where

import Prelude hiding (Enum)
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype Enum = Enum
  { value :: Word16
  } deriving (Eq, Show)

$(deriveJson ''Enum)

bitPut :: Enum -> BitPut.BitPut
bitPut enumAttribute =
  BitPut.bits 11 (value enumAttribute)

bitGet :: BitGet Enum
bitGet = Enum <$> getBitsLE 11
