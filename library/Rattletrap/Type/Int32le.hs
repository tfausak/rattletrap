{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int32le
  ( Int32le(..)
  ) where

import Rattletrap.Type.Common

newtype Int32le = Int32le
  { int32leValue :: Int32
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int32le)
