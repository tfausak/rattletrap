{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PlayerHistoryKeyAttribute
  ( PlayerHistoryKeyAttribute(..)
  ) where

import Rattletrap.Type.Common

newtype PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { playerHistoryKeyAttributeUnknown :: Word16
  } deriving (Eq, Ord, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)
