{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Location where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype LocationAttribute = LocationAttribute
  { value :: Vector.Vector
  } deriving (Eq, Show)

$(deriveJson ''LocationAttribute)

bitPut :: LocationAttribute -> BitPut ()
bitPut locationAttribute =
  Vector.bitPut (value locationAttribute)

bitGet :: (Int, Int, Int) -> BitGet LocationAttribute
bitGet version =
  LocationAttribute <$> Vector.bitGet version
