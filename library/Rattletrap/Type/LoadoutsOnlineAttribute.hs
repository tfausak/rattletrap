{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutsOnlineAttribute
  ( LoadoutsOnlineAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.LoadoutOnlineAttribute

data LoadoutsOnlineAttribute = LoadoutsOnlineAttribute
  { loadoutsOnlineAttributeBlue :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeOrange :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeUnknown1 :: Bool
  , loadoutsOnlineAttributeUnknown2 :: Bool
  } deriving (Eq, Ord, Show)

$(deriveJson ''LoadoutsOnlineAttribute)
