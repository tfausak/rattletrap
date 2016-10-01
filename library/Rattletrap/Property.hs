{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Property where

import Rattletrap.PropertyValue
import Rattletrap.Text
import Rattletrap.Word64

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data Property = Property
  { propertyKind :: Text
  , propertySize :: Word64
  , propertyValue :: PropertyValue Property
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Property

instance Aeson.ToJSON Property

getProperty :: Binary.Get Property
getProperty = do
  kind <- getText
  size <- getWord64
  value <- getPropertyValue getProperty kind size
  pure
    Property {propertyKind = kind, propertySize = size, propertyValue = value}

putProperty :: Property -> Binary.Put
putProperty property = do
  putText (propertyKind property)
  putWord64 (propertySize property)
  putPropertyValue putProperty (propertyValue property)
