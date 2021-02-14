{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Enum where

import Prelude hiding (Enum)
import Rattletrap.Type.Common
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

newtype Enum = Enum
  { value :: Word16
  } deriving (Eq, Show)

$(deriveJson ''Enum)

bitPut :: Enum -> BitPut.BitPut
bitPut enumAttribute =
  BitPut.bits 11 (value enumAttribute)

bitGet :: BitGet.BitGet Enum
bitGet = Enum <$> BitGet.bits 11
