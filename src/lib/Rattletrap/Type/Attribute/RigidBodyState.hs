{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.RigidBodyState where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data RigidBodyState = RigidBodyState
  { sleeping :: Bool
  , location :: Vector.Vector
  , rotation :: Rotation.Rotation
  , linearVelocity :: Maybe Vector.Vector
  , angularVelocity :: Maybe Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''RigidBodyState)

bitPut :: RigidBodyState -> BitPut ()
bitPut rigidBodyStateAttribute = do
  BinaryBits.putBool (sleeping rigidBodyStateAttribute)
  Vector.bitPut (location rigidBodyStateAttribute)
  Rotation.bitPut (rotation rigidBodyStateAttribute)
  case linearVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just x -> Vector.bitPut x
  case angularVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just x -> Vector.bitPut x

bitGet
  :: (Int, Int, Int) -> BitGet RigidBodyState
bitGet version = do
  sleeping_ <- getBool
  RigidBodyState sleeping_
    <$> Vector.bitGet version
    <*> Rotation.bitGet version
    <*> decodeWhen (not sleeping_) (Vector.bitGet version)
    <*> decodeWhen (not sleeping_) (Vector.bitGet version)
