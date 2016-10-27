module Rattletrap.AttributeValueType where

data AttributeValueType
  = AVBoolean
  | AVByte
  | AVCamSettings
  | AVDemolish
  | AVEnum
  | AVExplosion
  | AVFlaggedInt
  | AVFloat
  | AVGameMode
  | AVInt
  | AVLoadout
  | AVLoadoutOnline
  | AVLoadouts
  | AVLoadoutsOnline
  | AVLocation
  | AVMusicStinger
  | AVPartyLeader
  | AVPickup
  | AVPrivateMatchSettings
  | AVQWord
  | AVReservation
  | AVRigidBodyState
  | AVString
  | AVTeamPaint
  | AVUniqueId
  | AVWeldedInfo
  deriving (Eq, Ord, Show)
