{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rattletrap.JSON where

import Rattletrap.Attribute
import Rattletrap.AttributeMapping
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
import Rattletrap.PropertyValue
import Rattletrap.Replay
import Rattletrap.Replication
import Rattletrap.ReplicationValue
import Rattletrap.Rotation
import Rattletrap.Text
import Rattletrap.Word32
import Rattletrap.Word64
import Rattletrap.Word8

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

$(Monad.foldM
    (\declarations name -> do
       newDeclarations <- Aeson.deriveJSON Aeson.defaultOptions name
       pure (newDeclarations ++ declarations))
    []
    [ ''Attribute
    , ''AttributeValue
    , ''Cache
    , ''ClassMapping
    , ''CompressedWord
    , ''Content
    , ''Dictionary
    , ''Float32
    , ''Frame
    , ''Header
    , ''Initialization
    , ''Int32
    , ''Int8
    , ''KeyFrame
    , ''List
    , ''Location
    , ''Mark
    , ''Message
    , ''Property
    , ''AttributeMapping
    , ''PropertyValue
    , ''Replay
    , ''Replication
    , ''ReplicationValue
    , ''Rotation
    , ''Text
    , ''Word32
    , ''Word64
    , ''Word8
    ])
