module Rattletrap.Encode.RigidBodyStateAttribute
  ( putRigidBodyStateAttribute
  )
where

import Rattletrap.Encode.Rotation
import Rattletrap.Encode.Vector
import Rattletrap.Type.RigidBodyStateAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

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
