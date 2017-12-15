{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.GameModeAttribute
  ( GameModeAttribute(..)
  ) where

import Rattletrap.Type.Common

data GameModeAttribute = GameModeAttribute
  { gameModeAttributeNumBits :: Int
  , gameModeAttributeWord :: Word8
  } deriving (Eq, Ord, Show)

$(deriveJson ''GameModeAttribute)
