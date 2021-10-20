module Rattletrap.Type.Attribute.Location where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

newtype Location = Location
  { value :: Vector.Vector
  } deriving (Eq, Show)

instance Json.FromValue Location where
  fromValue = fmap Location . Json.fromValue

instance Json.ToValue Location where
  toValue = Json.toValue . value

schema :: Schema.Schema
schema = Schema.named "attribute-location" $ Schema.ref Vector.schema

bitPut :: Location -> BitPut.BitPut
bitPut locationAttribute = Vector.bitPut (value locationAttribute)

bitGet :: Version.Version -> BitGet.BitGet Location
bitGet version = BitGet.label "Location" $ do
  value <- BitGet.label "value" $ Vector.bitGet version
  pure Location { value }
