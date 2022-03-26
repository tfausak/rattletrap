module Rattletrap.Type.RemoteId.QQ where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

newtype QQ
  = QQ U64.U64
  deriving (Eq, Show)

instance Json.FromJSON QQ where
  parseJSON = fmap fromU64 . Json.parseJSON

instance Json.ToJSON QQ where
  toJSON = Json.toJSON . toU64

fromU64 :: U64.U64 -> QQ
fromU64 = QQ

toU64 :: QQ -> U64.U64
toU64 (QQ x) = x

schema :: Schema.Schema
schema = U64.schema

bitPut :: QQ -> BitPut.BitPut
bitPut = U64.bitPut . toU64

bitGet :: BitGet.BitGet QQ
bitGet = BitGet.label "QQ" $ fmap fromU64 U64.bitGet
