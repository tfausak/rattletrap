module Rattletrap.Type.Attribute.String where

import Prelude hiding (String)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Type.Str as Str

newtype String = String
  { value :: Str.Str
  } deriving (Eq, Show)

instance Json.FromJSON String where
  parseJSON = fmap String . Json.parseJSON

instance Json.ToJSON String where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-string" $ Schema.ref Str.schema

bitPut :: String -> BitPut.BitPut
bitPut stringAttribute = Str.bitPut (value stringAttribute)

bitGet :: BitGet.BitGet String
bitGet = String <$> Str.bitGet
