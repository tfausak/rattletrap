{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Vector
  ( Vector(..)
  ) where

import Rattletrap.Type.Common

data Vector = Vector
  { vectorBias :: Word
  , vectorX :: Int
  , vectorY :: Int
  , vectorZ :: Int
  } deriving (Eq, Ord, Show)

$(deriveJson ''Vector)
