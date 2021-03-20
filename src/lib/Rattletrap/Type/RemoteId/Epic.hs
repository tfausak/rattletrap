module Rattletrap.Type.RemoteId.Epic where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

newtype Epic
  = Epic Str.Str
  deriving (Eq, Show)

instance Json.FromJSON Epic where
  parseJSON = fmap fromStr . Json.parseJSON

instance Json.ToJSON Epic where
  toJSON = Json.toJSON . toStr

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
