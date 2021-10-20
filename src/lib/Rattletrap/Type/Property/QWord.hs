module Rattletrap.Type.Property.QWord where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

newtype QWord
  = QWord U64.U64
  deriving (Eq, Show)

fromU64 :: U64.U64 -> QWord
fromU64 = QWord

toU64 :: QWord -> U64.U64
toU64 (QWord x) = x

instance Json.FromValue QWord where
  fromValue = fmap fromU64 . Json.fromValue

instance Json.ToValue QWord where
  toValue = Json.toValue . toU64

schema :: Schema.Schema
schema = U64.schema

bytePut :: QWord -> BytePut.BytePut
bytePut = U64.bytePut . toU64

byteGet :: ByteGet.ByteGet QWord
byteGet = ByteGet.label "QWord" $ fmap fromU64 U64.byteGet
