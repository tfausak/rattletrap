{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeType
  ( AttributeType(..)
  ) where

import Rattletrap.Type.Common

data AttributeType
  = AttributeTypeAppliedDamage
  | AttributeTypeBoolean
  | AttributeTypeByte
  | AttributeTypeCamSettings
  | AttributeTypeClubColors
  | AttributeTypeDamageState
  | AttributeTypeDemolish
  | AttributeTypeEnum
  | AttributeTypeExplosion
  | AttributeTypeExtendedExplosion
  | AttributeTypeFlaggedInt
  | AttributeTypeFloat
  | AttributeTypeGameMode
  | AttributeTypeInt
  | AttributeTypeLoadout
  | AttributeTypeLoadoutOnline
  | AttributeTypeLoadouts
  | AttributeTypeLoadoutsOnline
  | AttributeTypeLocation
  | AttributeTypeMusicStinger
  | AttributeTypePartyLeader
  | AttributeTypePickup
  | AttributeTypePlayerHistoryKey
  | AttributeTypePrivateMatchSettings
  | AttributeTypeQWord
  | AttributeTypeReservation
  | AttributeTypeRigidBodyState
  | AttributeTypeStatEvent
  | AttributeTypeString
  | AttributeTypeTeamPaint
  | AttributeTypeUniqueId
  | AttributeTypeWeldedInfo
  deriving (Eq, Ord, Show)

$(deriveJson ''AttributeType)
