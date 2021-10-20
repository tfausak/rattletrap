module Rattletrap.Type.RemoteId.Epic where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

newtype Epic
  = Epic Str.Str
  deriving (Eq, Show)

instance Json.FromValue Epic where
  fromValue = fmap fromStr . Json.fromValue

instance Json.ToValue Epic where
  toValue = Json.toValue . toStr

fromStr :: Str.Str -> Epic
fromStr = Epic

toStr :: Epic -> Str.Str
toStr (Epic x) = x

schema :: Schema.Schema
schema = Str.schema

bitPut :: Epic -> BitPut.BitPut
bitPut = Str.bitPut . toStr

bitGet :: BitGet.BitGet Epic
bitGet = BitGet.label "Epic" $ fmap fromStr Str.bitGet
