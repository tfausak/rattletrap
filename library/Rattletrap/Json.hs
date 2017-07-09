{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rattletrap.Json where

import Rattletrap.Attribute
import Rattletrap.AttributeMapping
import Rattletrap.AttributeValue
import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.Content
import Rattletrap.Frame
import Rattletrap.Header
import Rattletrap.Initialization
import Rattletrap.KeyFrame
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Primitive
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.RemoteId
import Rattletrap.Replay
import Rattletrap.Replication
import Rattletrap.ReplicationValue

import qualified Control.Monad as Monad
import qualified Data.Aeson.Casing as Casing
import qualified Data.Aeson.TH as Aeson
import qualified Language.Haskell.TH as TH

$(let optionsFor name =
        (Casing.aesonDrop (length (TH.nameBase name)) Casing.snakeCase)
        { Aeson.constructorTagModifier = Casing.snakeCase
        , Aeson.omitNothingFields = True
        , Aeson.sumEncoding = Aeson.ObjectWithSingleField
        , Aeson.unwrapUnaryRecords = True
        }
      deriveJSON declarations name = do
        newDeclarations <- Aeson.deriveJSON (optionsFor name) name
        pure (newDeclarations ++ declarations)
      names =
        [ ''AppliedDamageAttribute
        , ''Attribute
        , ''AttributeMapping
        , ''AttributeValue
        , ''BooleanAttribute
        , ''ByteAttribute
        , ''Cache
        , ''CamSettingsAttribute
        , ''ClassMapping
        , ''ClubColorsAttribute
        , ''CompressedWord
        , ''CompressedWordVector
        , ''Content
        , ''DamageStateAttribute
        , ''DemolishAttribute
        , ''DestroyedReplication
        , ''Dictionary
        , ''EnumAttribute
        , ''ExplosionAttribute
        , ''ExtendedExplosionAttribute
        , ''FlaggedIntAttribute
        , ''Float32
        , ''FloatAttribute
        , ''Frame
        , ''GameModeAttribute
        , ''Header
        , ''Initialization
        , ''Int32
        , ''Int8
        , ''Int8Vector
        , ''IntAttribute
        , ''KeyFrame
        , ''List
        , ''LoadoutAttribute
        , ''LoadoutOnlineAttribute
        , ''LoadoutsAttribute
        , ''LoadoutsOnlineAttribute
        , ''LocationAttribute
        , ''Mark
        , ''Message
        , ''MusicStingerAttribute
        , ''PartyLeaderAttribute
        , ''PickupAttribute
        , ''PrivateMatchSettingsAttribute
        , ''Property
        , ''PropertyValue
        , ''QWordAttribute
        , ''RemoteId
        , ''Replay
        , ''Replication
        , ''ReplicationValue
        , ''ReservationAttribute
        , ''RigidBodyStateAttribute
        , ''Section
        , ''SpawnedReplication
        , ''StringAttribute
        , ''TeamPaintAttribute
        , ''Text
        , ''UniqueIdAttribute
        , ''UpdatedReplication
        , ''Vector
        , ''WeldedInfoAttribute
        , ''Word32
        , ''Word64
        , ''Word8
        ]
  in Monad.foldM deriveJSON [] names)
