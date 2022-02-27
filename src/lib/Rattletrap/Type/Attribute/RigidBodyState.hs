module Rattletrap.Type.Attribute.RigidBodyState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data RigidBodyState = RigidBodyState
  { sleeping :: Bool
  , location :: Vector.Vector
  , rotation :: Rotation.Rotation
  , linearVelocity :: Maybe Vector.Vector
  , angularVelocity :: Maybe Vector.Vector
  }
  deriving (Eq, Show)

instance Argo.HasCodec RigidBodyState where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ RigidBodyState
      <$> Argo.required sleeping "sleeping"
      <*> Argo.required location "location"
      <*> Argo.required rotation "rotation"
      <*> Argo.optional linearVelocity "linear_velocity"
      <*> Argo.optional angularVelocity "angular_velocity"

bitPut :: RigidBodyState -> BitPut.BitPut
bitPut rigidBodyStateAttribute =
  BitPut.bool (sleeping rigidBodyStateAttribute)
    <> Vector.bitPut (location rigidBodyStateAttribute)
    <> Rotation.bitPut (rotation rigidBodyStateAttribute)
    <> foldMap Vector.bitPut (linearVelocity rigidBodyStateAttribute)
    <> foldMap Vector.bitPut (angularVelocity rigidBodyStateAttribute)

bitGet :: Version.Version -> BitGet.BitGet RigidBodyState
bitGet version = BitGet.label "RigidBodyState" $ do
  sleeping <- BitGet.label "sleeping" BitGet.bool
  location <- BitGet.label "location" $ Vector.bitGet version
  rotation <- BitGet.label "rotation" $ Rotation.bitGet version
  linearVelocity <- BitGet.label "linearVelocity"
    $ Monad.whenMaybe (not sleeping) (Vector.bitGet version)
  angularVelocity <- BitGet.label "angularVelocity"
    $ Monad.whenMaybe (not sleeping) (Vector.bitGet version)
  pure RigidBodyState
    { sleeping
    , location
    , rotation
    , linearVelocity
    , angularVelocity
    }
