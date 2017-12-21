{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.EnumAttribute
  ( EnumAttribute(..)
  ) where

import Rattletrap.Type.Common

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word16
  } deriving (Eq, Ord, Show)

$(deriveJson ''EnumAttribute)
