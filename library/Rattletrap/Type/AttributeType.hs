{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeType
  ( AttributeType(..)
  ) where

import Rattletrap.Type.Common

data AttributeType
  = AppliedDamageAttributeType
  | BooleanAttributeType
  | ByteAttributeType
  | CamSettingsAttributeType
  | ClubColorsAttributeType
  | DamageStateAttributeType
  | DemolishAttributeType
  | EnumAttributeType
  | ExplosionAttributeType
  | ExtendedExplosionAttributeType
  | FlaggedIntAttributeType
  | FloatAttributeType
  | GameModeAttributeType
  | IntAttributeType
  | LoadoutAttributeType
  | LoadoutOnlineAttributeType
  | LoadoutsAttributeType
  | LoadoutsOnlineAttributeType
  | LocationAttributeType
  | MusicStingerAttributeType
  | PartyLeaderAttributeType
  | PickupAttributeType
  | PrivateMatchSettingsAttributeType
  | QWordAttributeType
  | ReservationAttributeType
  | RigidBodyStateAttributeType
  | StringAttributeType
  | TeamPaintAttributeType
  | UniqueIdAttributeType
  | WeldedInfoAttributeType
  deriving (Eq, Ord, Show)

$(deriveJson ''AttributeType)
