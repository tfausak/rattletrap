module Rattletrap.Type.Attribute.Float where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Utility.Json as Json
import Prelude hiding (Float)

newtype Float = Float
  { value :: F32.F32
  }
  deriving (Eq, Show)

instance Json.FromJSON Float where
  parseJSON = fmap Float . Json.parseJSON

instance Json.ToJSON Float where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-float" $ Schema.ref F32.schema

bitPut :: Float -> BitPut.BitPut
bitPut floatAttribute = F32.bitPut (value floatAttribute)

bitGet :: BitGet.BitGet Float
bitGet = BitGet.label "Float" $ do
  value <- BitGet.label "value" F32.bitGet
  pure Float {value}
