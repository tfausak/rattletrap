module Rattletrap.Type.LoadoutOnlineAttribute
  ( LoadoutOnlineAttribute(..)
  ) where

import Rattletrap.Type.ProductAttribute

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Ord, Show)
