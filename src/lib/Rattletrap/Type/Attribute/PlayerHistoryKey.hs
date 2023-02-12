module Rattletrap.Type.Attribute.PlayerHistoryKey where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype PlayerHistoryKey = PlayerHistoryKey
  { unknown :: Word.Word16
  }
  deriving (Eq, Show)

instance Json.FromJSON PlayerHistoryKey where
  parseJSON = fmap PlayerHistoryKey . Json.parseJSON

instance Json.ToJSON PlayerHistoryKey where
  toJSON = Json.toJSON . unknown

schema :: Schema.Schema
schema =
  Schema.named "attribute-player-history-key" $ Schema.ref Schema.number

bitPut :: PlayerHistoryKey -> BitPut.BitPut
bitPut = BitPut.bits 14 . unknown

bitGet :: BitGet.BitGet PlayerHistoryKey
bitGet = BitGet.label "PlayerHistoryKey" $ do
  unknown <- BitGet.label "unknown" $ BitGet.bits 14
  pure PlayerHistoryKey {unknown}
