module Rattletrap.Type.Attribute.Int where

import Prelude hiding (Int)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json

newtype Int = Int
  { value :: I32.I32
  } deriving (Eq, Show)

instance Json.FromValue Int where
  fromValue = fmap Int . Json.fromValue

instance Json.ToValue Int where
  toValue = Json.toValue . value

schema :: Schema.Schema
schema = Schema.named "attribute-int" $ Schema.ref I32.schema

bitPut :: Int -> BitPut.BitPut
bitPut intAttribute = I32.bitPut (value intAttribute)

bitGet :: BitGet.BitGet Int
bitGet = BitGet.label "Int" $ do
  value <- BitGet.label "value" I32.bitGet
  pure Int { value }
