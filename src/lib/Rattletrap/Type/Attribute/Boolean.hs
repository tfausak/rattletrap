module Rattletrap.Type.Attribute.Boolean where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

instance Json.FromValue Boolean where
  fromValue = fmap Boolean . Json.fromValue

instance Json.ToValue Boolean where
  toValue = Json.toValue . value

schema :: Schema.Schema
schema = Schema.named "attribute-boolean" $ Schema.ref Schema.boolean

bitPut :: Boolean -> BitPut.BitPut
bitPut booleanAttribute = BitPut.bool (value booleanAttribute)

bitGet :: BitGet.BitGet Boolean
bitGet = BitGet.label "Boolean" $ do
  value <- BitGet.label "value" BitGet.bool
  pure Boolean { value }
