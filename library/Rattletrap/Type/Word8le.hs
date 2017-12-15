{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word8le
  ( Word8le(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype Word8le = Word8le
  { word8leValue :: Word.Word8
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word8le)
