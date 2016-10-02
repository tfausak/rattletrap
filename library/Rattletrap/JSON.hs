{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rattletrap.JSON where

import Rattletrap.Attribute
import Rattletrap.AttributeValue
import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.CompressedWord
import Rattletrap.Content
import Rattletrap.Dictionary
import Rattletrap.Float32
import Rattletrap.Frame
import Rattletrap.Header
import Rattletrap.Initialization
import Rattletrap.Int32
import Rattletrap.Int8
import Rattletrap.KeyFrame
import Rattletrap.List
import Rattletrap.Location
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Property
import Rattletrap.PropertyMapping
import Rattletrap.PropertyValue
import Rattletrap.Replay
import Rattletrap.Replication
import Rattletrap.ReplicationValue
import Rattletrap.Rotation
import Rattletrap.Text
import Rattletrap.Word32
import Rattletrap.Word64
import Rattletrap.Word8

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics

deriving instance Generics.Generic Attribute

instance Aeson.FromJSON Attribute

instance Aeson.ToJSON Attribute

deriving instance Generics.Generic AttributeValue

instance Aeson.FromJSON AttributeValue

instance Aeson.ToJSON AttributeValue

deriving instance Generics.Generic Cache

instance Aeson.FromJSON Cache

instance Aeson.ToJSON Cache

deriving instance Generics.Generic ClassMapping

instance Aeson.FromJSON ClassMapping

instance Aeson.ToJSON ClassMapping

deriving instance Generics.Generic CompressedWord

instance Aeson.FromJSON CompressedWord

instance Aeson.ToJSON CompressedWord

deriving instance Generics.Generic Content

instance Aeson.FromJSON Content

instance Aeson.ToJSON Content

deriving instance Generics.Generic (Dictionary a)

instance Aeson.FromJSON a =>
         Aeson.FromJSON (Dictionary a)

instance Aeson.ToJSON a =>
         Aeson.ToJSON (Dictionary a)

deriving instance Generics.Generic Float32

instance Aeson.FromJSON Float32

instance Aeson.ToJSON Float32

deriving instance Generics.Generic Frame

instance Aeson.FromJSON Frame

instance Aeson.ToJSON Frame

deriving instance Generics.Generic Header

instance Aeson.FromJSON Header

instance Aeson.ToJSON Header

deriving instance Generics.Generic Initialization

instance Aeson.FromJSON Initialization

instance Aeson.ToJSON Initialization

deriving instance Generics.Generic Int32

instance Aeson.FromJSON Int32

instance Aeson.ToJSON Int32

deriving instance Generics.Generic Int8

instance Aeson.FromJSON Int8

instance Aeson.ToJSON Int8

deriving instance Generics.Generic KeyFrame

instance Aeson.FromJSON KeyFrame

instance Aeson.ToJSON KeyFrame

deriving instance Generics.Generic (List a)

instance Aeson.FromJSON a =>
         Aeson.FromJSON (List a)

instance Aeson.ToJSON a =>
         Aeson.ToJSON (List a)

deriving instance Generics.Generic Location

instance Aeson.FromJSON Location

instance Aeson.ToJSON Location

deriving instance Generics.Generic Mark

instance Aeson.FromJSON Mark

instance Aeson.ToJSON Mark

deriving instance Generics.Generic Message

instance Aeson.FromJSON Message

instance Aeson.ToJSON Message

deriving instance Generics.Generic Property

instance Aeson.FromJSON Property

instance Aeson.ToJSON Property

deriving instance Generics.Generic PropertyMapping

instance Aeson.FromJSON PropertyMapping

instance Aeson.ToJSON PropertyMapping

deriving instance Generics.Generic (PropertyValue a)

instance Aeson.FromJSON a =>
         Aeson.FromJSON (PropertyValue a)

instance Aeson.ToJSON a =>
         Aeson.ToJSON (PropertyValue a)

deriving instance Generics.Generic Replay

instance Aeson.FromJSON Replay

instance Aeson.ToJSON Replay

deriving instance Generics.Generic Replication

instance Aeson.FromJSON Replication

instance Aeson.ToJSON Replication

deriving instance Generics.Generic ReplicationValue

instance Aeson.FromJSON ReplicationValue

instance Aeson.ToJSON ReplicationValue

deriving instance Generics.Generic Rotation

instance Aeson.FromJSON Rotation

instance Aeson.ToJSON Rotation

deriving instance Generics.Generic Text

instance Aeson.FromJSON Text

instance Aeson.ToJSON Text

deriving instance Generics.Generic Word32

instance Aeson.FromJSON Word32

instance Aeson.ToJSON Word32

deriving instance Generics.Generic Word64

instance Aeson.FromJSON Word64

instance Aeson.ToJSON Word64

deriving instance Generics.Generic Word8

instance Aeson.FromJSON Word8

instance Aeson.ToJSON Word8
