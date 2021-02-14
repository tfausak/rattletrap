{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PlayerHistoryKey where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

newtype PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { unknown :: Word16
  } deriving (Eq, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)

bitPut
  :: PlayerHistoryKeyAttribute -> BitPut ()
bitPut = putBitsLE 14 . unknown

bitGet :: BitGet PlayerHistoryKeyAttribute
bitGet =
  PlayerHistoryKeyAttribute <$> getBitsLE 14
