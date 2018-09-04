{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word8le
  ( Word8le(..)
  )
where

import Rattletrap.Type.Common

newtype Word8le = Word8le
  { word8leValue :: Word8
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word8le)
