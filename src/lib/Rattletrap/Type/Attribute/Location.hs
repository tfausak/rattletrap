module Rattletrap.Type.Attribute.Location where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Schema as Schema

newtype Location = Location
  { value :: Vector.Vector
  } deriving (Eq, Show)

instance Json.FromJSON Location where
  parseJSON = fmap Location . Json.parseJSON

instance Json.ToJSON Location where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-location" $ Schema.ref Vector.schema

bitPut :: Location -> BitPut.BitPut
bitPut locationAttribute = Vector.bitPut (value locationAttribute)

bitGet :: Version.Version -> BitGet.BitGet Location
bitGet version = Location <$> Vector.bitGet version
