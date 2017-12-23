{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PlayerHistoryKeyAttribute
  ( PlayerHistoryKeyAttribute(..)
  ) where

import Rattletrap.Type.Common

data PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { playerHistoryKeyAttributeValue :: Word16
  } deriving (Eq, Ord, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)
