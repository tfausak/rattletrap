{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FloatAttribute
  ( FloatAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''FloatAttribute)
