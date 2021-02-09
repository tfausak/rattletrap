{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Vector
  ( Vector(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord

data Vector = Vector
  { vectorSize :: CompressedWord
  , vectorBias :: Word
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Word' rather than something more precise like a
  -- 'Word8' because it just gets passed to a functions that expect 'Word's.
  -- There's no reason to do a bunch of conversions.
  , vectorX :: Int
  -- ^ See 'vectorBias'.
  , vectorY :: Int
  -- ^ See 'vectorBias'.
  , vectorZ :: Int
  -- ^ See 'vectorBias'.
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Vector)
