{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word32le
  ( Word32le(..)
  ) where

import Rattletrap.Type.Common

newtype Word32le = Word32le
  { word32leValue :: Word32
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word32le)
