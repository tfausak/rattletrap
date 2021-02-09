{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8le
  ( Int8le(..)
  ) where

import Rattletrap.Type.Common

newtype Int8le = Int8le
  { int8leValue :: Int8
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int8le)
