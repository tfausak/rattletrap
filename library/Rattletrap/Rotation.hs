{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Rotation where

import Rattletrap.Int8

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified GHC.Generics as Generics

data Rotation = Rotation
  { rotationX :: Maybe Int8
  , rotationY :: Maybe Int8
  , rotationZ :: Maybe Int8
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Rotation

instance Aeson.ToJSON Rotation

getRotation :: BinaryBit.BitGet Rotation
getRotation = do
  x <- getRotationField
  y <- getRotationField
  z <- getRotationField
  pure Rotation {rotationX = x, rotationY = y, rotationZ = z}

putRotation :: Rotation -> BinaryBit.BitPut ()
putRotation rotation = do
  putRotationField (rotationX rotation)
  putRotationField (rotationY rotation)
  putRotationField (rotationZ rotation)

getRotationField :: BinaryBit.BitGet (Maybe Int8)
getRotationField = do
  hasField <- BinaryBit.getBool
  if hasField
    then do
      field <- getInt8Bits
      pure (Just field)
    else pure Nothing

putRotationField :: Maybe Int8 -> BinaryBit.BitPut ()
putRotationField maybeField =
  case maybeField of
    Nothing -> BinaryBit.putBool False
    Just field -> do
      BinaryBit.putBool True
      putInt8Bits field
