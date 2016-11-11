module Rattletrap.Attribute.RigidBodyState where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data RigidBodyStateAttribute = RigidBodyStateAttribute
  { rigidBodyStateAttributeSleeping :: Bool
  , rigidBodyStateAttributeLocation :: Vector
  , rigidBodyStateAttributeRotation :: CompressedWordVector
  , rigidBodyStateAttributeLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeAngularVelocity :: Maybe Vector
  } deriving (Eq, Ord, Show)

getRigidBodyStateAttribute :: BinaryBit.BitGet RigidBodyStateAttribute
getRigidBodyStateAttribute = do
  sleeping <- BinaryBit.getBool
  location <- getVector
  rotation <- getCompressedWordVector
  linearVelocity <-
    if sleeping
      then pure Nothing
      else do
        linearVelocity <- getVector
        pure (Just linearVelocity)
  angularVelocity <-
    if sleeping
      then pure Nothing
      else do
        angularVelocity <- getVector
        pure (Just angularVelocity)
  pure
    (RigidBodyStateAttribute
       sleeping
       location
       rotation
       linearVelocity
       angularVelocity)

putRigidBodyStateAttribute :: RigidBodyStateAttribute
                                -> BinaryBit.BitPut ()
putRigidBodyStateAttribute rigidBodyStateAttribute = do
  BinaryBit.putBool
    (rigidBodyStateAttributeSleeping rigidBodyStateAttribute)
  putVector (rigidBodyStateAttributeLocation rigidBodyStateAttribute)
  putCompressedWordVector
    (rigidBodyStateAttributeRotation rigidBodyStateAttribute)
  case rigidBodyStateAttributeLinearVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just linearVelocity -> putVector linearVelocity
  case rigidBodyStateAttributeAngularVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just angularVelocity -> putVector angularVelocity
