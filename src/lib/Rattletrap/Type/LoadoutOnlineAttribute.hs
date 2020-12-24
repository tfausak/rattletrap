{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutOnlineAttribute
  ( LoadoutOnlineAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.ProductAttribute

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Ord, Show)

$(deriveJson ''LoadoutOnlineAttribute)
