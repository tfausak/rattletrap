{-# LANGUAGE TemplateHaskell #-}

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
  = AttributeValueAppliedDamage AppliedDamageAttribute
  | AttributeValueBoolean BooleanAttribute
  | AttributeValueByte ByteAttribute
  | AttributeValueCamSettings CamSettingsAttribute
  | AttributeValueClubColors ClubColorsAttribute
  | AttributeValueDamageState DamageStateAttribute
  | AttributeValueDemolish DemolishAttribute
  | AttributeValueEnum EnumAttribute
  | AttributeValueExplosion ExplosionAttribute
  | AttributeValueExtendedExplosion ExtendedExplosionAttribute
  | AttributeValueFlaggedInt FlaggedIntAttribute
  | AttributeValueFloat FloatAttribute
  | AttributeValueGameMode GameModeAttribute
  | AttributeValueInt IntAttribute
  | AttributeValueLoadout LoadoutAttribute
  | AttributeValueLoadoutOnline LoadoutOnlineAttribute
  | AttributeValueLoadouts LoadoutsAttribute
  | AttributeValueLoadoutsOnline LoadoutsOnlineAttribute
  | AttributeValueLocation LocationAttribute
  | AttributeValueMusicStinger MusicStingerAttribute
  | AttributeValuePartyLeader PartyLeaderAttribute
  | AttributeValuePickup PickupAttribute
  | AttributeValuePrivateMatchSettings PrivateMatchSettingsAttribute
  | AttributeValueQWord QWordAttribute
  | AttributeValueReservation ReservationAttribute
  | AttributeValueRigidBodyState RigidBodyStateAttribute
  | AttributeValueString StringAttribute
  | AttributeValueTeamPaint TeamPaintAttribute
  | AttributeValueUniqueId UniqueIdAttribute
  | AttributeValueWeldedInfo WeldedInfoAttribute
  deriving (Eq, Ord, Show)

$(deriveJson ''AttributeValue)
