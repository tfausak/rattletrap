{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Float where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype FloatAttribute = FloatAttribute
  { value :: F32.F32
  } deriving (Eq, Show)

$(deriveJson ''FloatAttribute)

bitPut :: FloatAttribute -> BitPut ()
bitPut floatAttribute =
  F32.bitPut (value floatAttribute)

bitGet :: BitGet FloatAttribute
bitGet = FloatAttribute <$> F32.bitGet
