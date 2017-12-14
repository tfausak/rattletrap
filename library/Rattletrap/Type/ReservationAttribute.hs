module Rattletrap.Type.ReservationAttribute
  ( ReservationAttribute(..)
  ) where

import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Type.Text
import Rattletrap.Type.CompressedWord

import qualified Data.Word as Word

data ReservationAttribute = ReservationAttribute
  { reservationAttributeNumber :: CompressedWord
  , reservationAttributeUniqueId :: UniqueIdAttribute
  , reservationAttributeName :: Maybe Text
  , reservationAttributeUnknown1 :: Bool
  , reservationAttributeUnknown2 :: Bool
  , reservationAttributeUnknown3 :: Maybe Word.Word8
  } deriving (Eq, Ord, Show)
