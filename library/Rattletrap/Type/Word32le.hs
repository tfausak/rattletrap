{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word32le
  ( Word32le(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype Word32le = Word32le
  { word32leValue :: Word.Word32
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word32le)
