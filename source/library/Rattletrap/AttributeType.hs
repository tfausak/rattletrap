module Rattletrap.AttributeType where

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
