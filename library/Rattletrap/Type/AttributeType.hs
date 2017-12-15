{-# LANGUAGE DeriveGeneric #-}

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
  | AttributeTypePrivateMatchSettings
  | AttributeTypeQWord
  | AttributeTypeReservation
  | AttributeTypeRigidBodyState
  | AttributeTypeString
  | AttributeTypeTeamPaint
  | AttributeTypeUniqueId
  | AttributeTypeWeldedInfo
  deriving (Eq, Generic, Ord, Show)

instance FromJSON AttributeType where
  parseJSON = defaultParseJson "AttributeType"

instance ToJSON AttributeType where
  toEncoding = defaultToEncoding "AttributeType"
  toJSON = defaultToJson "AttributeType"
