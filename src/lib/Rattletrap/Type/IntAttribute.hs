{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.IntAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le

import qualified Data.Binary.Bits.Put as BinaryBits

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''IntAttribute)

putIntAttribute :: IntAttribute -> BinaryBits.BitPut ()
putIntAttribute intAttribute = putInt32Bits (intAttributeValue intAttribute)
