{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.RigidBodyState where

import Rattletrap.Type.Common
import Rattletrap.Type.Rotation
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector.Vector
  , rigidBodyStateAttributeRotation :: Rotation
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector.Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''RigidBodyStateAttribute)

putRigidBodyStateAttribute :: RigidBodyStateAttribute -> BitPut ()
putRigidBodyStateAttribute rigidBodyStateAttribute = do
  BinaryBits.putBool (rigidBodyStateAttributeSleeping rigidBodyStateAttribute)
  Vector.bitPut (rigidBodyStateAttributeLocation rigidBodyStateAttribute)
  putRotation (rigidBodyStateAttributeRotation rigidBodyStateAttribute)
  case rigidBodyStateAttributeLinearVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just linearVelocity -> Vector.bitPut linearVelocity
  case rigidBodyStateAttributeAngularVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just angularVelocity -> Vector.bitPut angularVelocity

decodeRigidBodyStateAttributeBits
  :: (Int, Int, Int) -> BitGet RigidBodyStateAttribute
decodeRigidBodyStateAttributeBits version = do
  sleeping <- getBool
  RigidBodyStateAttribute sleeping
    <$> Vector.bitGet version
    <*> decodeRotationBits version
    <*> decodeWhen (not sleeping) (Vector.bitGet version)
    <*> decodeWhen (not sleeping) (Vector.bitGet version)
