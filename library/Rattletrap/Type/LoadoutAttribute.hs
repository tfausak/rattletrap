{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.LoadoutAttribute
  ( LoadoutAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Word8

data LoadoutAttribute = LoadoutAttribute
  { loadoutAttributeVersion :: Word8
  , loadoutAttributeBody :: Word32
  , loadoutAttributeDecal :: Word32
  , loadoutAttributeWheels :: Word32
  , loadoutAttributeRocketTrail :: Word32
  -- ^ Now known as "rocket boost".
  , loadoutAttributeAntenna :: Word32
  , loadoutAttributeTopper :: Word32
  , loadoutAttributeUnknown1 :: Word32
  , loadoutAttributeUnknown2 :: Maybe Word32
  , loadoutAttributeEngineAudio :: Maybe Word32
  , loadoutAttributeTrail :: Maybe Word32
  , loadoutAttributeGoalExplosion :: Maybe Word32
  , loadoutAttributeBanner :: Maybe Word32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON LoadoutAttribute where
  parseJSON = defaultParseJson "LoadoutAttribute"

instance ToJSON LoadoutAttribute where
  toEncoding = defaultToEncoding "LoadoutAttribute"
  toJSON = defaultToJson "LoadoutAttribute"
