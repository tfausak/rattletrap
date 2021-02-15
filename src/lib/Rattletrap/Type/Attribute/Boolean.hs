{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Boolean where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

$(deriveJson ''Boolean)

bitPut :: Boolean -> BitPut.BitPut
bitPut booleanAttribute = BitPut.bool (value booleanAttribute)

bitGet :: BitGet.BitGet Boolean
bitGet = Boolean <$> BitGet.bool
