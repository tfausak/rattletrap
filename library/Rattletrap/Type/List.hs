module Rattletrap.Type.List
  ( List(..)
  ) where

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Ord, Show)
