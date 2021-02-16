module Rattletrap.Type.Attribute.Location where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector

newtype Location = Location
  { value :: Vector.Vector
  } deriving (Eq, Show)

$(deriveJson ''Location)

bitPut :: Location -> BitPut.BitPut
bitPut locationAttribute = Vector.bitPut (value locationAttribute)

bitGet :: (Int, Int, Int) -> BitGet.BitGet Location
bitGet version = Location <$> Vector.bitGet version
