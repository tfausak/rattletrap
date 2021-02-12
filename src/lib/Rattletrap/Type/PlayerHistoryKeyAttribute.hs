{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PlayerHistoryKeyAttribute where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { playerHistoryKeyAttributeUnknown :: Word16
  } deriving (Eq, Ord, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)

putPlayerHistoryKeyAttribute
  :: PlayerHistoryKeyAttribute -> BinaryBits.BitPut ()
putPlayerHistoryKeyAttribute = putBitsLE 14 . playerHistoryKeyAttributeUnknown
