{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.AttributeValue
  ( AttributeValue(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.AppliedDamageAttribute
import Rattletrap.Type.BooleanAttribute
import Rattletrap.Type.ByteAttribute
import Rattletrap.Type.CamSettingsAttribute
import Rattletrap.Type.ClubColorsAttribute
import Rattletrap.Type.DamageStateAttribute
import Rattletrap.Type.DemolishAttribute
import Rattletrap.Type.EnumAttribute
import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Type.ExtendedExplosionAttribute
import Rattletrap.Type.FlaggedIntAttribute
import Rattletrap.Type.FloatAttribute
import Rattletrap.Type.GameModeAttribute
import Rattletrap.Type.IntAttribute
import Rattletrap.Type.LoadoutAttribute
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Type.LocationAttribute
import Rattletrap.Type.MusicStingerAttribute
import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Type.PickupAttribute
import Rattletrap.Type.PrivateMatchSettingsAttribute
import Rattletrap.Type.QWordAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Type.RigidBodyStateAttribute
import Rattletrap.Type.StringAttribute
import Rattletrap.Type.TeamPaintAttribute
import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Type.WeldedInfoAttribute

data AttributeValue
  = AppliedDamageAttributeValue AppliedDamageAttribute
  | BooleanAttributeValue BooleanAttribute
  | ByteAttributeValue ByteAttribute
  | CamSettingsAttributeValue CamSettingsAttribute
  | ClubColorsAttributeValue ClubColorsAttribute
  | DamageStateAttributeValue DamageStateAttribute
  | DemolishAttributeValue DemolishAttribute
  | EnumAttributeValue EnumAttribute
  | ExplosionAttributeValue ExplosionAttribute
  | ExtendedExplosionAttributeValue ExtendedExplosionAttribute
  | FlaggedIntAttributeValue FlaggedIntAttribute
  | FloatAttributeValue FloatAttribute
  | GameModeAttributeValue GameModeAttribute
  | IntAttributeValue IntAttribute
  | LoadoutAttributeValue LoadoutAttribute
  | LoadoutOnlineAttributeValue LoadoutOnlineAttribute
  | LoadoutsAttributeValue LoadoutsAttribute
  | LoadoutsOnlineAttributeValue LoadoutsOnlineAttribute
  | LocationAttributeValue LocationAttribute
  | MusicStingerAttributeValue MusicStingerAttribute
  | PartyLeaderAttributeValue PartyLeaderAttribute
  | PickupAttributeValue PickupAttribute
  | PrivateMatchSettingsAttributeValue PrivateMatchSettingsAttribute
  | QWordAttributeValue QWordAttribute
  | ReservationAttributeValue ReservationAttribute
  | RigidBodyStateAttributeValue RigidBodyStateAttribute
  | StringAttributeValue StringAttribute
  | TeamPaintAttributeValue TeamPaintAttribute
  | UniqueIdAttributeValue UniqueIdAttribute
  | WeldedInfoAttributeValue WeldedInfoAttribute
  deriving (Eq, Generic, Ord, Show)

instance FromJSON AttributeValue where
  parseJSON = defaultParseJson "AttributeValue"

instance ToJSON AttributeValue where
  toEncoding = defaultToEncoding "AttributeValue"
  toJSON = defaultToJson "AttributeValue"
