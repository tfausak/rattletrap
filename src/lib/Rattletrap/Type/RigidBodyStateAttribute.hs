{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RigidBodyStateAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Rotation
import Rattletrap.Type.Vector

import qualified Data.Binary.Bits.Put as BinaryBits

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector
  , rigidBodyStateAttributeRotation :: Rotation
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''RigidBodyStateAttribute)

putRigidBodyStateAttribute :: RigidBodyStateAttribute -> BinaryBits.BitPut ()
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
