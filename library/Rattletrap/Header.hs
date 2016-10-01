{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Header where

import Rattletrap.Dictionary
import Rattletrap.Property
import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data Header = Header
  { headerEngineVersion :: Word32
  , headerLicenseeVersion :: Word32
  , headerLabel :: Text
  , headerProperties :: Dictionary Property
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Header

instance Aeson.ToJSON Header

getHeader :: Binary.Get Header
getHeader = do
  engineVersion <- getWord32
  licenseeVersion <- getWord32
  label <- getText
  properties <- getDictionary getProperty
  pure
    Header
    { headerEngineVersion = engineVersion
    , headerLicenseeVersion = licenseeVersion
    , headerLabel = label
    , headerProperties = properties
    }

putHeader :: Header -> Binary.Put
putHeader header = do
  putWord32 (headerEngineVersion header)
  putWord32 (headerLicenseeVersion header)
  putText (headerLabel header)
  putDictionary putProperty (headerProperties header)
