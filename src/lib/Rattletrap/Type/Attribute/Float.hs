{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Float where

import Prelude hiding (Float)
import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype Float = Float
  { value :: F32.F32
  } deriving (Eq, Show)

$(deriveJson ''Float)

bitPut :: Float -> BitPut.BitPut
bitPut floatAttribute =
  F32.bitPut (value floatAttribute)

bitGet :: BitGet Float
bitGet = Float <$> F32.bitGet
