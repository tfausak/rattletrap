{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FloatAttribute
  ( FloatAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32
  } deriving (Eq, Ord, Show)

$(deriveJson ''FloatAttribute)
