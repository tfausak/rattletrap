module Rattletrap.Type.Property.Name where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

newtype Name
  = Name Str.Str
  deriving (Eq, Show)

fromStr :: Str.Str -> Name
fromStr = Name

toStr :: Name -> Str.Str
toStr (Name x) = x

instance Json.FromValue Name where
  fromValue = fmap fromStr . Json.fromValue

instance Json.ToValue Name where
  toValue = Json.toValue . toStr

schema :: Schema.Schema
schema = Str.schema

bytePut :: Name -> BytePut.BytePut
bytePut = Str.bytePut . toStr

byteGet :: ByteGet.ByteGet Name
byteGet = ByteGet.label "Name" $ fmap fromStr Str.byteGet
