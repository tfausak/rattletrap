module Rattletrap.Type.BooleanAttribute
  ( BooleanAttribute(..)
  ) where

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Ord, Show)
