module Rattletrap.Type.Attribute.PlayerHistoryKey where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Schema as Schema

newtype PlayerHistoryKey = PlayerHistoryKey
  { unknown :: Word.Word16
  } deriving (Eq, Show)

$(deriveJson ''PlayerHistoryKey)

schema :: Schema.Schema
schema = Schema.named "attribute-player-history-key" $ Schema.ref Schema.number

bitPut :: PlayerHistoryKey -> BitPut.BitPut
bitPut = BitPut.bits 14 . unknown

bitGet :: BitGet.BitGet PlayerHistoryKey
bitGet = PlayerHistoryKey <$> BitGet.bits 14
