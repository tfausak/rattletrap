{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PlayerHistoryKey where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

newtype PlayerHistoryKey = PlayerHistoryKey
  { unknown :: Word16
  } deriving (Eq, Show)

$(deriveJson ''PlayerHistoryKey)

bitPut
  :: PlayerHistoryKey -> BitPut ()
bitPut = putBitsLE 14 . unknown

bitGet :: BitGet PlayerHistoryKey
bitGet =
  PlayerHistoryKey <$> getBitsLE 14
