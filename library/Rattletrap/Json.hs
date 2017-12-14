{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rattletrap.Json where

import Rattletrap.Type.Attribute
import Rattletrap.Type.AppliedDamageAttribute
import Rattletrap.Type.BooleanAttribute
import Rattletrap.Type.ByteAttribute
import Rattletrap.Type.CamSettingsAttribute
import Rattletrap.Type.ClubColorsAttribute
import Rattletrap.Type.DamageStateAttribute
import Rattletrap.Type.DemolishAttribute
import Rattletrap.Type.EnumAttribute
import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Type.ExtendedExplosionAttribute
import Rattletrap.Type.FlaggedIntAttribute
import Rattletrap.Type.FloatAttribute
import Rattletrap.Type.GameModeAttribute
import Rattletrap.Type.IntAttribute
import Rattletrap.Type.LoadoutAttribute
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Type.LocationAttribute
import Rattletrap.Type.MusicStingerAttribute
import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Type.PickupAttribute
import Rattletrap.Type.PrivateMatchSettingsAttribute
import Rattletrap.Type.ProductAttribute
import Rattletrap.Type.QWordAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Type.RigidBodyStateAttribute
import Rattletrap.Type.StringAttribute
import Rattletrap.Type.TeamPaintAttribute
import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Type.WeldedInfoAttribute
import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.AttributeValue
import Rattletrap.Type.Cache
import Rattletrap.Type.ClassMapping
import Rattletrap.Type.Content
import Rattletrap.Type.Frame
import Rattletrap.Type.Header
import Rattletrap.Type.Initialization
import Rattletrap.Type.KeyFrame
import Rattletrap.Type.Mark
import Rattletrap.Type.Message
import Rattletrap.Type.Dictionary
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.CompressedWordVector
import Rattletrap.Type.Int32
import Rattletrap.Type.Word8
import Rattletrap.Type.Word64
import Rattletrap.Type.Word32
import Rattletrap.Type.Vector
import Rattletrap.Type.Text
import Rattletrap.Type.List
import Rattletrap.Type.Int8Vector
import Rattletrap.Type.Int8
import Rattletrap.Type.Float32
import Rattletrap.Type.Section
import Rattletrap.Type.DestroyedReplication
import Rattletrap.Type.SpawnedReplication
import Rattletrap.Type.UpdatedReplication
import Rattletrap.Type.Property
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Replay
import Rattletrap.Type.Replication
import Rattletrap.Type.ReplicationValue

import qualified Control.Monad as Monad
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Language.Haskell.TH as TH

$(let toSnakeCase = Aeson.camelTo2 '_'
      dropName name = drop (length (TH.nameBase name))
      optionsFor name =
        Aeson.defaultOptions
        { Aeson.constructorTagModifier = toSnakeCase
        , Aeson.fieldLabelModifier = toSnakeCase . dropName name
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
        , ''ProductAttribute
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
