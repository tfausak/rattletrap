module Rattletrap.Type.Attribute.Rotation where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Utility.Json as Json

newtype Rotation = Rotation
  { value :: Int8Vector.Int8Vector
  } deriving (Eq, Show)

instance Json.FromJSON Rotation where
  parseJSON = fmap Rotation . Json.parseJSON

instance Json.ToJSON Rotation where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-rotation" $ Schema.ref Int8Vector.schema

bitPut :: Rotation -> BitPut.BitPut
bitPut = Int8Vector.bitPut . value

bitGet :: BitGet.BitGet Rotation
bitGet = BitGet.label "Rotation" $ do
  value <- BitGet.label "value" Int8Vector.bitGet
  pure Rotation { value }
