{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Rotation
  ( Rotation(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Quaternion
import Rattletrap.Type.CompressedWordVector

data Rotation
  = RotationCompressedWordVector CompressedWordVector
  | RotationQuaternion Quaternion
  deriving (Eq, Ord, Show)

$(deriveJson ''Rotation)
