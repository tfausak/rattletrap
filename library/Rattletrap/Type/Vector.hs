{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Vector
  ( Vector(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord

data Vector = Vector
  { vectorSize :: CompressedWord
  , vectorBias :: Word
  , vectorX :: Int
  , vectorY :: Int
  , vectorZ :: Int
  } deriving (Eq, Ord, Show)

$(deriveJson ''Vector)
