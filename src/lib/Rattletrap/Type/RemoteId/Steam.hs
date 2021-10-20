module Rattletrap.Type.RemoteId.Steam where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

newtype Steam
  = Steam U64.U64
  deriving (Eq, Show)

instance Json.FromValue Steam where
  fromValue = fmap fromU64 . Json.fromValue

instance Json.ToValue Steam where
  toValue = Json.toValue . toU64

fromU64 :: U64.U64 -> Steam
fromU64 = Steam

toU64 :: Steam -> U64.U64
toU64 (Steam x) = x

schema :: Schema.Schema
schema = U64.schema

bitPut :: Steam -> BitPut.BitPut
bitPut = U64.bitPut . toU64

bitGet :: BitGet.BitGet Steam
bitGet = BitGet.label "Steam" $ fmap fromU64 U64.bitGet
