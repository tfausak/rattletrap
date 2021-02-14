{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Float where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype FloatAttribute = FloatAttribute
  { value :: Float32le.Float32le
  } deriving (Eq, Show)

$(deriveJson ''FloatAttribute)

bitPut :: FloatAttribute -> BitPut ()
bitPut floatAttribute =
  Float32le.bitPut (value floatAttribute)

bitGet :: BitGet FloatAttribute
bitGet = FloatAttribute <$> Float32le.bitGet
