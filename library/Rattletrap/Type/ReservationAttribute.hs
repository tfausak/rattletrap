{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ReservationAttribute
  ( ReservationAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.UniqueIdAttribute

data ReservationAttribute = ReservationAttribute
  { reservationAttributeNumber :: CompressedWord
  , reservationAttributeUniqueId :: UniqueIdAttribute
  , reservationAttributeName :: Maybe Str
  , reservationAttributeUnknown1 :: Bool
  , reservationAttributeUnknown2 :: Bool
  , reservationAttributeUnknown3 :: Maybe Word8
  } deriving (Eq, Ord, Show)

$(deriveJson ''ReservationAttribute)
