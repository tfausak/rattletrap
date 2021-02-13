{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeType where

import Rattletrap.Type.Common

data AttributeType
  = AppliedDamage
  | Boolean
  | Byte
  | CamSettings
  | ClubColors
  | CustomDemolish
  | DamageState
  | Demolish
  | Enum
  | Explosion
  | ExtendedExplosion
  | FlaggedInt
  | FlaggedByte
  | Float
  | GameMode
  | Int
  | Int64
  | Loadout
  | LoadoutOnline
  | Loadouts
  | LoadoutsOnline
  | Location
  | MusicStinger
  | PartyLeader
  | Pickup
  | PickupNew
  | PlayerHistoryKey
  | PrivateMatchSettings
  | QWord
  | Reservation
  | RigidBodyState
  | StatEvent
  | String
  | TeamPaint
  | Title
  | UniqueId
  | WeldedInfo
  deriving (Eq, Show)

$(deriveJsonWith ''AttributeType jsonOptions)
