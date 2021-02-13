{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Location where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype LocationAttribute = LocationAttribute
  { locationAttributeValue :: Vector
  } deriving (Eq, Show)

$(deriveJson ''LocationAttribute)

putLocationAttribute :: LocationAttribute -> BitPut ()
putLocationAttribute locationAttribute =
  putVector (locationAttributeValue locationAttribute)

decodeLocationAttributeBits :: (Int, Int, Int) -> BitGet LocationAttribute
decodeLocationAttributeBits version =
  LocationAttribute <$> decodeVectorBits version
