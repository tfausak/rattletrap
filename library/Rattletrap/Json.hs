{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rattletrap.Json where

import Rattletrap.Attribute
import Rattletrap.AttributeMapping
import Rattletrap.AttributeValue
import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.Primitive.CompressedWord
import Rattletrap.Primitive.CompressedWordVector
import Rattletrap.Content
import Rattletrap.Primitive.Dictionary
import Rattletrap.Primitive.Float32
import Rattletrap.Frame
import Rattletrap.Header
import Rattletrap.Initialization
import Rattletrap.Primitive.Int32
import Rattletrap.Primitive.Int8
import Rattletrap.Primitive.Int8Vector
import Rattletrap.KeyFrame
import Rattletrap.Primitive.List
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.RemoteId
import Rattletrap.Replay
import Rattletrap.Replication
import Rattletrap.ReplicationValue
import Rattletrap.Section
import Rattletrap.Text
import Rattletrap.Vector
import Rattletrap.Word32
import Rattletrap.Word64
import Rattletrap.Word8

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
        [ ''Attribute
        , ''AttributeMapping
        , ''AttributeValue
        , ''BooleanAttributeValue
        , ''ByteAttributeValue
        , ''Cache
        , ''CamSettingsAttributeValue
        , ''ClassMapping
        , ''ClubColorsAttributeValue
        , ''CompressedWord
        , ''CompressedWordVector
        , ''Content
        , ''DemolishAttributeValue
        , ''DestroyedReplicationValue
        , ''Dictionary
        , ''EnumAttributeValue
        , ''ExplosionAttributeValue
        , ''FlaggedIntAttributeValue
        , ''Float32
        , ''FloatAttributeValue
        , ''Frame
        , ''GameModeAttributeValue
        , ''Header
        , ''Initialization
        , ''Int32
        , ''Int8
        , ''Int8Vector
        , ''IntAttributeValue
        , ''KeyFrame
        , ''List
        , ''LoadoutAttributeValue
        , ''LoadoutOnlineAttributeValue
        , ''LoadoutsAttributeValue
        , ''LoadoutsOnlineAttributeValue
        , ''LocationAttributeValue
        , ''Mark
        , ''Message
        , ''MusicStingerAttributeValue
        , ''PartyLeaderAttributeValue
        , ''PickupAttributeValue
        , ''PrivateMatchSettingsAttributeValue
        , ''Property
        , ''PropertyValue
        , ''QWordAttributeValue
        , ''RemoteId
        , ''Replay
        , ''Replication
        , ''ReplicationValue
        , ''ReservationAttributeValue
        , ''RigidBodyStateAttributeValue
        , ''Section
        , ''SpawnedReplicationValue
        , ''StringAttributeValue
        , ''TeamPaintAttributeValue
        , ''Text
        , ''UniqueIdAttributeValue
        , ''UpdatedReplicationValue
        , ''Vector
        , ''WeldedInfoAttributeValue
        , ''Word32
        , ''Word64
        , ''Word8
        ]
  in Monad.foldM deriveJSON [] names)
