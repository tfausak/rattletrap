{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8
  ( Int8(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Int as Int

newtype Int8 = Int8
  { int8Value :: Int.Int8
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int8)
