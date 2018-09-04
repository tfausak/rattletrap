{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.BooleanAttribute
  ( BooleanAttribute(..)
  )
where

import Rattletrap.Type.Common

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Ord, Show)

$(deriveJson ''BooleanAttribute)
