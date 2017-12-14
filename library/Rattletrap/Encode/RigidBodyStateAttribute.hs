module Rattletrap.Encode.RigidBodyStateAttribute
  ( putRigidBodyStateAttribute
  ) where

import Rattletrap.Type.RigidBodyStateAttribute
import Rattletrap.Encode.Vector
import Rattletrap.Encode.CompressedWordVector

import qualified Data.Binary.Bits.Put as BinaryBit

putRigidBodyStateAttribute :: RigidBodyStateAttribute -> BinaryBit.BitPut ()
putRigidBodyStateAttribute rigidBodyStateAttribute = do
  BinaryBit.putBool (rigidBodyStateAttributeSleeping rigidBodyStateAttribute)
  putVector (rigidBodyStateAttributeLocation rigidBodyStateAttribute)
  putCompressedWordVector
    (rigidBodyStateAttributeRotation rigidBodyStateAttribute)
  case rigidBodyStateAttributeLinearVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just linearVelocity -> putVector linearVelocity
  case rigidBodyStateAttributeAngularVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just angularVelocity -> putVector angularVelocity
