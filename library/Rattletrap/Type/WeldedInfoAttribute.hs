{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.WeldedInfoAttribute
  ( WeldedInfoAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Int8Vector
import Rattletrap.Type.Vector

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32le
  , weldedInfoAttributeOffset :: Vector
  , weldedInfoAttributeMass :: Float32le
  , weldedInfoAttributeRotation :: Int8Vector
  } deriving (Eq, Ord, Show)

$(deriveJson ''WeldedInfoAttribute)
