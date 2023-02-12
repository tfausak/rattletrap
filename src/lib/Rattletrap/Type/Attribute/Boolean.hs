module Rattletrap.Type.Attribute.Boolean where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype Boolean = Boolean
  { value :: Bool
  }
  deriving (Eq, Show)

instance Json.FromJSON Boolean where
  parseJSON = fmap Boolean . Json.parseJSON

instance Json.ToJSON Boolean where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-boolean" $ Schema.ref Schema.boolean

bitPut :: Boolean -> BitPut.BitPut
bitPut booleanAttribute = BitPut.bool (value booleanAttribute)

bitGet :: BitGet.BitGet Boolean
bitGet = BitGet.label "Boolean" $ do
  value <- BitGet.label "value" BitGet.bool
  pure Boolean {value}
