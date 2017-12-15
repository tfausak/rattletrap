{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word64le
  ( Word64le(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype Word64le = Word64le
  { word64leValue :: Word.Word64
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word64le)
