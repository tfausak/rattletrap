{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Float32le
  ( Float32le(..)
  ) where

import Rattletrap.Type.Common

newtype Float32le = Float32le
  { float32leValue :: Float
  } deriving (Eq, Ord, Show)

$(deriveJson ''Float32le)
