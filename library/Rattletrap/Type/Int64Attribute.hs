{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int64Attribute
  ( Int64Attribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int64le

newtype Int64Attribute = Int64Attribute
  { int64AttributeValue :: Int64le
  } deriving (Eq, Ord, Show)

$(deriveJson ''IntAttribute)
