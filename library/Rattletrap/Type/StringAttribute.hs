{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.StringAttribute
  ( StringAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Text

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Text
  } deriving (Eq, Ord, Show)

$(deriveJson ''StringAttribute)
