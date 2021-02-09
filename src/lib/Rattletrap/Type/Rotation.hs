{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Rotation
  ( Rotation(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWordVector
import Rattletrap.Type.Quaternion

data Rotation
  = RotationCompressedWordVector CompressedWordVector
  | RotationQuaternion Quaternion
  deriving (Eq, Ord, Show)

$(deriveJson ''Rotation)
