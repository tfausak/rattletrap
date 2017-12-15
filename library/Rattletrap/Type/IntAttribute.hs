{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.IntAttribute
  ( IntAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''IntAttribute)
