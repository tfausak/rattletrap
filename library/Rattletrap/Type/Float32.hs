{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Float32
  ( Float32(..)
  ) where

import Rattletrap.Type.Common

newtype Float32 = Float32
  { float32Value :: Float
  } deriving (Eq, Ord, Show)

$(deriveJson ''Float32)
