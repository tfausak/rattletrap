module Rattletrap.Type.Property.Bool where

import Prelude hiding (Bool)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

newtype Bool
  = Bool U8.U8
  deriving (Eq, Show)

fromU8 :: U8.U8 -> Bool
fromU8 = Bool

toU8 :: Bool -> U8.U8
toU8 (Bool x) = x

instance Json.FromJSON Bool where
  parseJSON = fmap fromU8 . Json.parseJSON

instance Json.ToJSON Bool where
  toJSON = Json.toJSON . toU8

schema :: Schema.Schema
schema = U8.schema

bytePut :: Bool -> BytePut.BytePut
bytePut = U8.bytePut . toU8

byteGet :: ByteGet.ByteGet Bool
byteGet = ByteGet.label "Bool" $ fmap fromU8 U8.byteGet
