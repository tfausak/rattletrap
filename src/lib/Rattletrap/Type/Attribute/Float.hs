module Rattletrap.Type.Attribute.Float where

import Prelude hiding (Float)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Schema as Schema

newtype Float = Float
  { value :: F32.F32
  } deriving (Eq, Show)

$(deriveJson ''Float)

schema :: Schema.Schema
schema = Schema.named "attribute-float" $ Schema.ref F32.schema

bitPut :: Float -> BitPut.BitPut
bitPut floatAttribute = F32.bitPut (value floatAttribute)

bitGet :: BitGet.BitGet Float
bitGet = Float <$> F32.bitGet
