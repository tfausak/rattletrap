module Rattletrap.Type.Property.Str where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

newtype Str
  = Str Str.Str
  deriving (Eq, Show)

fromStr :: Str.Str -> Str
fromStr = Str

toStr :: Str -> Str.Str
toStr (Str x) = x

instance Json.FromValue Str where
  fromValue = fmap fromStr . Json.fromValue

instance Json.ToValue Str where
  toValue = Json.toValue . toStr

schema :: Schema.Schema
schema = Str.schema

bytePut :: Str -> BytePut.BytePut
bytePut = Str.bytePut . toStr

byteGet :: ByteGet.ByteGet Str
byteGet = ByteGet.label "Str" $ fmap fromStr Str.byteGet
