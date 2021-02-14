{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Boolean where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

$(deriveJson ''Boolean)

bitPut :: Boolean -> BitPut.BitPut
bitPut booleanAttribute =
  BitPut.bool (value booleanAttribute)

bitGet :: BitGet Boolean
bitGet = Boolean <$> getBool
