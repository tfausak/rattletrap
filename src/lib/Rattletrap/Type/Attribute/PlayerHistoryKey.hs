{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PlayerHistoryKey where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common

newtype PlayerHistoryKey = PlayerHistoryKey
  { unknown :: Word16
  } deriving (Eq, Show)

$(deriveJson ''PlayerHistoryKey)

bitPut :: PlayerHistoryKey -> BitPut.BitPut
bitPut = BitPut.bits 14 . unknown

bitGet :: BitGet.BitGet PlayerHistoryKey
bitGet = PlayerHistoryKey <$> BitGet.bits 14
