{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PlayerHistoryKeyAttribute
  ( PlayerHistoryKeyAttribute(..)
  ) where

import Rattletrap.Type.Bitstream
import Rattletrap.Type.Common

data PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { playerHistoryKeyAttributeUnknown :: Bitstream
  } deriving (Eq, Ord, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)
