{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.StringAttribute
  ( StringAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Str

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Str
  } deriving (Eq, Ord, Show)

$(deriveJson ''StringAttribute)
