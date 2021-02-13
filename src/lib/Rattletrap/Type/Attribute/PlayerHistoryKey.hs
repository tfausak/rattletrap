{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PlayerHistoryKey where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

newtype PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { playerHistoryKeyAttributeUnknown :: Word16
  } deriving (Eq, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)

putPlayerHistoryKeyAttribute
  :: PlayerHistoryKeyAttribute -> BitPut ()
putPlayerHistoryKeyAttribute = putBitsLE 14 . playerHistoryKeyAttributeUnknown

decodePlayerHistoryKeyAttributeBits :: BitGet PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits =
  PlayerHistoryKeyAttribute <$> getBitsLE 14
