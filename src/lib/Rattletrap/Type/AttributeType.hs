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
  | AttributeTypeCustomDemolish
  | AttributeTypeDamageState
  | AttributeTypeDemolish
  | AttributeTypeEnum
  | AttributeTypeExplosion
  | AttributeTypeExtendedExplosion
  | AttributeTypeFlaggedInt
  | AttributeTypeFlaggedByte
  | AttributeTypeFloat
  | AttributeTypeGameMode
  | AttributeTypeInt
  | AttributeTypeInt64
  | AttributeTypeLoadout
  | AttributeTypeLoadoutOnline
  | AttributeTypeLoadouts
  | AttributeTypeLoadoutsOnline
  | AttributeTypeLocation
  | AttributeTypeMusicStinger
  | AttributeTypePartyLeader
  | AttributeTypePickup
  | AttributeTypePickupNew
  | AttributeTypePlayerHistoryKey
  | AttributeTypePrivateMatchSettings
  | AttributeTypeQWord
  | AttributeTypeReservation
  | AttributeTypeRigidBodyState
  | AttributeTypeStatEvent
  | AttributeTypeString
  | AttributeTypeTeamPaint
  | AttributeTypeTitle
  | AttributeTypeUniqueId
  | AttributeTypeWeldedInfo
  deriving (Eq, Ord, Show)

$(deriveJson ''AttributeType)
