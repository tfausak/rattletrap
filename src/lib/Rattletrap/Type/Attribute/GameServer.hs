module Rattletrap.Type.Attribute.GameServer where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.QWord as QWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

newtype GameServer
  = GameServer QWord.QWord
  deriving (Eq, Show)

instance Json.FromJSON GameServer where
  parseJSON = fmap GameServer . Json.parseJSON

instance Json.ToJSON GameServer where
  toJSON (GameServer x) = Json.toJSON x

schema :: Schema.Schema
schema = Schema.named "attribute-game-server" $ Schema.ref QWord.schema

bitPut :: GameServer -> BitPut.BitPut
bitPut (GameServer x) = QWord.bitPut x

bitGet :: Maybe Str.Str -> BitGet.BitGet GameServer
bitGet buildVersion = BitGet.label "GameServer" $ fmap GameServer QWord.bitGet
