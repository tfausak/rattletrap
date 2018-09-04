{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutAttribute
  ( LoadoutAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

data LoadoutAttribute = LoadoutAttribute
  { loadoutAttributeVersion :: Word8le
  , loadoutAttributeBody :: Word32le
  , loadoutAttributeDecal :: Word32le
  , loadoutAttributeWheels :: Word32le
  , loadoutAttributeRocketTrail :: Word32le
  -- ^ Now known as "rocket boost".
  , loadoutAttributeAntenna :: Word32le
  , loadoutAttributeTopper :: Word32le
  , loadoutAttributeUnknown1 :: Word32le
  , loadoutAttributeUnknown2 :: Maybe Word32le
  , loadoutAttributeEngineAudio :: Maybe Word32le
  , loadoutAttributeTrail :: Maybe Word32le
  , loadoutAttributeGoalExplosion :: Maybe Word32le
  , loadoutAttributeBanner :: Maybe Word32le
  , loadoutAttributeUnknown3 :: Maybe Word32le
  , loadoutAttributeUnknown4 :: Maybe Word32le
  , loadoutAttributeUnknown5 :: Maybe Word32le
  , loadoutAttributeUnknown6 :: Maybe Word32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''LoadoutAttribute)
