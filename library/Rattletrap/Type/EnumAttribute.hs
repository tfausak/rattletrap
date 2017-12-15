{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.EnumAttribute
  ( EnumAttribute(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word.Word16
  } deriving (Eq, Ord, Show)

$(deriveJson ''EnumAttribute)
