module Rattletrap.Type.RemoteId.Steam where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

newtype Steam
  = Steam U64.U64
  deriving (Eq, Show)

instance Json.FromJSON Steam where
  parseJSON = fmap fromU64 . Json.parseJSON

instance Json.ToJSON Steam where
  toJSON = Json.toJSON . toU64

fromU64 :: U64.U64 -> Steam
fromU64 = Steam

toU64 :: Steam -> U64.U64
toU64 (Steam x) = x

schema :: Schema.Schema
schema = U64.schema

bitPut :: Steam -> BitPut.BitPut
bitPut = U64.bitPut . toU64

bitGet :: BitGet.BitGet Steam
bitGet = fmap fromU64 U64.bitGet
