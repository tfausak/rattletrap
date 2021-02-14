{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.RigidBodyState where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

data RigidBodyState = RigidBodyState
  { sleeping :: Bool
  , location :: Vector.Vector
  , rotation :: Rotation.Rotation
  , linearVelocity :: Maybe Vector.Vector
  , angularVelocity :: Maybe Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''RigidBodyState)

bitPut :: RigidBodyState -> BitPut.BitPut
bitPut rigidBodyStateAttribute = do
  BitPut.bool (sleeping rigidBodyStateAttribute)
  Vector.bitPut (location rigidBodyStateAttribute)
  Rotation.bitPut (rotation rigidBodyStateAttribute)
  case linearVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just x -> Vector.bitPut x
  case angularVelocity rigidBodyStateAttribute of
    Nothing -> pure ()
    Just x -> Vector.bitPut x

bitGet
  :: (Int, Int, Int) -> BitGet.BitGet RigidBodyState
bitGet version = do
  sleeping_ <- BitGet.bool
  RigidBodyState sleeping_
    <$> Vector.bitGet version
    <*> Rotation.bitGet version
    <*> decodeWhen (not sleeping_) (Vector.bitGet version)
    <*> decodeWhen (not sleeping_) (Vector.bitGet version)
