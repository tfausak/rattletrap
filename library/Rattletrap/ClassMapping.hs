{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.ClassMapping where

import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data ClassMapping = ClassMapping
  { classMappingName :: Text
  , classMappingStreamId :: Word32
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON ClassMapping

instance Aeson.ToJSON ClassMapping

getClassMapping :: Binary.Get ClassMapping
getClassMapping = do
  name <- getText
  streamId <- getWord32
  pure ClassMapping {classMappingName = name, classMappingStreamId = streamId}

putClassMapping :: ClassMapping -> Binary.Put
putClassMapping classMapping = do
  putText (classMappingName classMapping)
  putWord32 (classMappingStreamId classMapping)
