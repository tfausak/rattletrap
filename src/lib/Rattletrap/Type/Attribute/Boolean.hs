{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Boolean where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

$(deriveJson ''Boolean)

bitPut :: Boolean -> BitPut ()
bitPut booleanAttribute =
  BinaryBits.putBool (value booleanAttribute)

bitGet :: BitGet Boolean
bitGet = Boolean <$> getBool
