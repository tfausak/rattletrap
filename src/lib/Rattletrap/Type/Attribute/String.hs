module Rattletrap.Type.Attribute.String where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json
import Prelude hiding (String)

newtype String = String
  { value :: Str.Str
  }
  deriving (Eq, Show)

instance Json.FromJSON String where
  parseJSON = fmap String . Json.parseJSON

instance Json.ToJSON String where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-string" $ Schema.ref Str.schema

bitPut :: String -> BitPut.BitPut
bitPut stringAttribute = Str.bitPut (value stringAttribute)

bitGet :: BitGet.BitGet String
bitGet = BitGet.label "String" $ do
  value <- BitGet.label "value" Str.bitGet
  pure String {value}
