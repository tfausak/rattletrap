{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Location where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype Location = Location
  { value :: Vector.Vector
  } deriving (Eq, Show)

$(deriveJson ''Location)

bitPut :: Location -> BitPut.BitPut
bitPut locationAttribute =
  Vector.bitPut (value locationAttribute)

bitGet :: (Int, Int, Int) -> BitGet Location
bitGet version =
  Location <$> Vector.bitGet version
