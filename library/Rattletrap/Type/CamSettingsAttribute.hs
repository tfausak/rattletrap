module Rattletrap.Type.CamSettingsAttribute
  ( CamSettingsAttribute(..)
  ) where

import Rattletrap.Type.Float32

data CamSettingsAttribute = CamSettingsAttribute
  { camSettingsAttributeFov :: Float32
  , camSettingsAttributeHeight :: Float32
  , camSettingsAttributeAngle :: Float32
  , camSettingsAttributeDistance :: Float32
  , camSettingsAttributeStiffness :: Float32
  , camSettingsAttributeSwivelSpeed :: Float32
  , camSettingsAttributeTransitionSpeed :: Maybe Float32
  } deriving (Eq, Ord, Show)
