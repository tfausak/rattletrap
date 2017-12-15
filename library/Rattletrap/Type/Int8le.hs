{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8le
  ( Int8le(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Int as Int

newtype Int8le = Int8le
  { int8Value :: Int.Int8
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int8le)
