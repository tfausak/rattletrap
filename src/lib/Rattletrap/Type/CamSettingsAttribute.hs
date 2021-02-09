{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CamSettingsAttribute
  ( CamSettingsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le

data CamSettingsAttribute = CamSettingsAttribute
  { camSettingsAttributeFov :: Float32le
  , camSettingsAttributeHeight :: Float32le
  , camSettingsAttributeAngle :: Float32le
  , camSettingsAttributeDistance :: Float32le
  , camSettingsAttributeStiffness :: Float32le
  , camSettingsAttributeSwivelSpeed :: Float32le
  , camSettingsAttributeTransitionSpeed :: Maybe Float32le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''CamSettingsAttribute)
