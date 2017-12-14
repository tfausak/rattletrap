module Rattletrap.Type.LoadoutsAttribute
  ( LoadoutsAttribute(..)
  ) where

import Rattletrap.Type.LoadoutAttribute

data LoadoutsAttribute = LoadoutsAttribute
  { loadoutsAttributeBlue :: LoadoutAttribute
  , loadoutsAttributeOrange :: LoadoutAttribute
  } deriving (Eq, Ord, Show)
