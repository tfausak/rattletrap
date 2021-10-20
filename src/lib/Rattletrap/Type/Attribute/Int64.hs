module Rattletrap.Type.Attribute.Int64 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I64 as I64
import qualified Rattletrap.Utility.Json as Json

newtype Int64 = Int64
  { value :: I64.I64
  } deriving (Eq, Show)

instance Json.FromValue Int64 where
  fromValue = fmap Int64 . Json.fromValue

instance Json.ToValue Int64 where
  toValue = Json.toValue . value

schema :: Schema.Schema
schema = Schema.named "attribute-int64" $ Schema.ref I64.schema

putInt64Attribute :: Int64 -> BitPut.BitPut
putInt64Attribute int64Attribute = I64.bitPut (value int64Attribute)

bitGet :: BitGet.BitGet Int64
bitGet = BitGet.label "Int64" $ do
  value <- BitGet.label "value" I64.bitGet
  pure Int64 { value }
