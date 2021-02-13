{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Location where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype LocationAttribute = LocationAttribute
  { locationAttributeValue :: Vector.Vector
  } deriving (Eq, Show)

$(deriveJson ''LocationAttribute)

putLocationAttribute :: LocationAttribute -> BitPut ()
putLocationAttribute locationAttribute =
  Vector.bitPut (locationAttributeValue locationAttribute)

decodeLocationAttributeBits :: (Int, Int, Int) -> BitGet LocationAttribute
decodeLocationAttributeBits version =
  LocationAttribute <$> Vector.bitGet version
