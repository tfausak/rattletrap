{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.GameModeAttribute
  ( GameModeAttribute(..)
  ) where

import Rattletrap.Type.Common

data GameModeAttribute = GameModeAttribute
  { gameModeAttributeNumBits :: Int
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Int' rather than something more precise like an
  -- 'Int8' because it just gets passed to functions that expect 'Int's.
  -- There's no reason to do a bunch of conversions.
  , gameModeAttributeWord :: Word8
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''GameModeAttribute)
