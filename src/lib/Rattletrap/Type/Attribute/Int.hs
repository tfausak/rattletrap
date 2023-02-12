module Rattletrap.Type.Attribute.Int where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json
import Prelude hiding (Int)

newtype Int = Int
  { value :: I32.I32
  }
  deriving (Eq, Show)

instance Json.FromJSON Int where
  parseJSON = fmap Int . Json.parseJSON

instance Json.ToJSON Int where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-int" $ Schema.ref I32.schema

bitPut :: Int -> BitPut.BitPut
bitPut intAttribute = I32.bitPut (value intAttribute)

bitGet :: BitGet.BitGet Int
bitGet = BitGet.label "Int" $ do
  value <- BitGet.label "value" I32.bitGet
  pure Int {value}
