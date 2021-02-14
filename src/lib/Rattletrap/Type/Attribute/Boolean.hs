{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Boolean where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype BooleanAttribute = BooleanAttribute
  { value :: Bool
  } deriving (Eq, Show)

$(deriveJsonWith ''BooleanAttribute jsonOptions)

bitPut :: BooleanAttribute -> BitPut ()
bitPut booleanAttribute =
  BinaryBits.putBool (value booleanAttribute)

bitGet :: BitGet BooleanAttribute
bitGet = BooleanAttribute <$> getBool
