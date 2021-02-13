{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LocationAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector

import qualified Data.Binary.Bits.Put as BinaryBits

newtype LocationAttribute = LocationAttribute
  { locationAttributeValue :: Vector
  } deriving (Eq, Ord, Show)

$(deriveJson ''LocationAttribute)

putLocationAttribute :: LocationAttribute -> BinaryBits.BitPut ()
putLocationAttribute locationAttribute =
  putVector (locationAttributeValue locationAttribute)
