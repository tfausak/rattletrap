{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.BooleanAttribute where

import Rattletrap.Type.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Ord, Show)

$(deriveJson ''BooleanAttribute)

putBooleanAttribute :: BooleanAttribute -> BinaryBits.BitPut ()
putBooleanAttribute booleanAttribute =
  BinaryBits.putBool (booleanAttributeValue booleanAttribute)
