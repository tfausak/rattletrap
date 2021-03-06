module Rattletrap.Type.Property.Byte where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data Byte = Byte
  { key :: Str.Str
  , value :: Maybe Str.Str
  }
  deriving (Eq, Show)

instance Json.FromJSON Byte where
  parseJSON json = do
    (key, value) <- Json.parseJSON json
    pure Byte { key, value }

instance Json.ToJSON Byte where
  toJSON byte = Json.toJSON (key byte, value byte)

schema :: Schema.Schema
schema = Schema.named "property-byte" $ Schema.tuple
  [Schema.ref Str.schema, Schema.json $ Schema.maybe Str.schema]

bytePut :: Byte -> BytePut.BytePut
bytePut byte = Str.bytePut (key byte) <> foldMap Str.bytePut (value byte)

byteGet :: ByteGet.ByteGet Byte
byteGet = do
  key <- Str.byteGet
  value <- Monad.whenMaybe (Str.toString key /= "OnlinePlatform_Steam")
    Str.byteGet
  pure Byte { key, value }
