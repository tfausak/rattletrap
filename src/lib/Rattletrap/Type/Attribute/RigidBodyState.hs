{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.RigidBodyState where

import Rattletrap.Type.Common
import Rattletrap.Type.Rotation
import Rattletrap.Type.Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector
  , rigidBodyStateAttributeRotation :: Rotation
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector
  }
  deriving (Eq, Show)

$(deriveJson ''RigidBodyStateAttribute)

putRigidBodyStateAttribute :: RigidBodyStateAttribute -> BitPut ()
putRigidBodyStateAttribute rigidBodyStateAttribute = do
  BinaryBits.putBool (rigidBodyStateAttributeSleeping rigidBodyStateAttribute)
  putVector (rigidBodyStateAttributeLocation rigidBodyStateAttribute)
  putRotation (rigidBodyStateAttributeRotation rigidBodyStateAttribute)
  case rigidBodyStateAttributeLinearVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just linearVelocity -> putVector linearVelocity
  case rigidBodyStateAttributeAngularVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just angularVelocity -> putVector angularVelocity

decodeRigidBodyStateAttributeBits
  :: (Int, Int, Int) -> BitGet RigidBodyStateAttribute
decodeRigidBodyStateAttributeBits version = do
  sleeping <- getBool
  RigidBodyStateAttribute sleeping
    <$> decodeVectorBits version
    <*> decodeRotationBits version
    <*> decodeWhen (not sleeping) (decodeVectorBits version)
    <*> decodeWhen (not sleeping) (decodeVectorBits version)
