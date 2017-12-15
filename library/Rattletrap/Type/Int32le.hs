{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int32le
  ( Int32le(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Int as Int

newtype Int32le = Int32le
  { int32leValue :: Int.Int32
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int32le)
