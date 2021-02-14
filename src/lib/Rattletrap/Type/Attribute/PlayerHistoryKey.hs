{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PlayerHistoryKey where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype PlayerHistoryKey = PlayerHistoryKey
  { unknown :: Word16
  } deriving (Eq, Show)

$(deriveJson ''PlayerHistoryKey)

bitPut
  :: PlayerHistoryKey -> BitPut.BitPut
bitPut = BitPut.bits 14 . unknown

bitGet :: BitGet PlayerHistoryKey
bitGet =
  PlayerHistoryKey <$> getBitsLE 14
