module Rattletrap.Type.Property.Int where

import Prelude hiding (Int)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json

newtype Int
  = Int I32.I32
  deriving (Eq, Show)

fromI32 :: I32.I32 -> Int
fromI32 = Int

toI32 :: Int -> I32.I32
toI32 (Int x) = x

instance Json.FromValue Int where
  fromValue = fmap fromI32 . Json.fromValue

instance Json.ToValue Int where
  toValue = Json.toValue . toI32

schema :: Schema.Schema
schema = I32.schema

bytePut :: Int -> BytePut.BytePut
bytePut = I32.bytePut . toI32

byteGet :: ByteGet.ByteGet Int
byteGet = ByteGet.label "I32" $ fmap fromI32 I32.byteGet
