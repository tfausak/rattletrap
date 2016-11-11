module Rattletrap.AttributeValue.RigidBodyState where

import Rattletrap.Primitive.CompressedWordVector
import Rattletrap.Primitive.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data RigidBodyStateAttributeValue = RigidBodyStateAttributeValue
  { rigidBodyStateAttributeValueSleeping :: Bool
  , rigidBodyStateAttributeValueLocation :: Vector
  , rigidBodyStateAttributeValueRotation :: CompressedWordVector
  , rigidBodyStateAttributeValueLinearVelocity :: Maybe Vector
  , rigidBodyStateAttributeValueAngularVelocity :: Maybe Vector
  } deriving (Eq, Ord, Show)

getRigidBodyStateAttributeValue :: BinaryBit.BitGet RigidBodyStateAttributeValue
getRigidBodyStateAttributeValue = do
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
    (RigidBodyStateAttributeValue
       sleeping
       location
       rotation
       linearVelocity
       angularVelocity)

putRigidBodyStateAttributeValue :: RigidBodyStateAttributeValue
                                -> BinaryBit.BitPut ()
putRigidBodyStateAttributeValue rigidBodyStateAttributeValue = do
  BinaryBit.putBool
    (rigidBodyStateAttributeValueSleeping rigidBodyStateAttributeValue)
  putVector (rigidBodyStateAttributeValueLocation rigidBodyStateAttributeValue)
  putCompressedWordVector
    (rigidBodyStateAttributeValueRotation rigidBodyStateAttributeValue)
  case rigidBodyStateAttributeValueLinearVelocity rigidBodyStateAttributeValue of
    Nothing -> pure ()
    Just linearVelocity -> putVector linearVelocity
  case rigidBodyStateAttributeValueAngularVelocity rigidBodyStateAttributeValue of
    Nothing -> pure ()
    Just angularVelocity -> putVector angularVelocity
