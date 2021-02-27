module Rattletrap.Type.Attribute.Boolean where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

$(deriveJson ''Boolean)

schema :: Schema.Schema
schema = Schema.named "attribute-boolean" $ Schema.json Schema.boolean

bitPut :: Boolean -> BitPut.BitPut
bitPut booleanAttribute = BitPut.bool (value booleanAttribute)

bitGet :: BitGet.BitGet Boolean
bitGet = Boolean <$> BitGet.bool
