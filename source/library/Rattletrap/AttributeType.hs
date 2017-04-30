module Rattletrap.AttributeType where

data AttributeType
  = BooleanAttributeType
  | ByteAttributeType
  | CamSettingsAttributeType
  | ClubColorsAttributeType
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
  | TitlesAttributeType
  | UniqueIdAttributeType
  | WeldedInfoAttributeType
  deriving (Eq, Ord, Show)
