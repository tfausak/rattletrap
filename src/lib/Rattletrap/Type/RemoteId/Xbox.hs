module Rattletrap.Type.RemoteId.Xbox where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

newtype Xbox
  = Xbox U64.U64
  deriving (Eq, Show)

instance Json.FromJSON Xbox where
  parseJSON = fmap fromU64 . Json.parseJSON

instance Json.ToJSON Xbox where
  toJSON = Json.toJSON . toU64

fromU64 :: U64.U64 -> Xbox
fromU64 = Xbox

toU64 :: Xbox -> U64.U64
toU64 (Xbox x) = x

schema :: Schema.Schema
schema = U64.schema

bitPut :: Xbox -> BitPut.BitPut
bitPut = U64.bitPut . toU64

bitGet :: BitGet.BitGet Xbox
bitGet = BitGet.label "Xbox" $ fmap fromU64 U64.bitGet
