{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Quaternion
  ( Quaternion(..)
  , Component(..)
  , toQuaternion
  , maxComponent
  )
where

import Rattletrap.Type.Common

data Quaternion = Quaternion
  { quaternionX :: Double
  , quaternionY :: Double
  , quaternionZ :: Double
  , quaternionW :: Double
  } deriving (Eq, Ord, Show)

$(deriveJson ''Quaternion)

data Component
  = ComponentX
  | ComponentY
  | ComponentZ
  | ComponentW
  deriving (Eq, Ord, Show)

toQuaternion :: Component -> Double -> Double -> Double -> Quaternion
toQuaternion component a b c =
  let d = sqrt (1 - (a * a) - (b * b) - (c * c))
  in
    case component of
      ComponentX -> Quaternion d a b c
      ComponentY -> Quaternion a d b c
      ComponentZ -> Quaternion a b d c
      ComponentW -> Quaternion a b c d

maxComponent :: Quaternion -> Component
maxComponent quaternion = snd
  (maximum
    (fmap
      (\(field, component) -> (field quaternion, component))
      [ (quaternionX, ComponentX)
      , (quaternionY, ComponentY)
      , (quaternionZ, ComponentZ)
      , (quaternionW, ComponentW)
      ]
    )
  )
