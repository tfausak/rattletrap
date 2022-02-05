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
  codec = Argo.identified .
    Argo.fromObjectCodec Argo.Allow
      $ RigidBodyState
      <$> Argo.project
            sleeping
            (Argo.required (Argo.fromString "sleeping") Argo.codec)
      <*> Argo.project
            location
            (Argo.required (Argo.fromString "location") Argo.codec)
      <*> Argo.project
            rotation
            (Argo.required (Argo.fromString "rotation") Argo.codec)
      <*> Argo.project
            linearVelocity
            (Argo.optional (Argo.fromString "linear_velocity") Argo.codec)
      <*> Argo.project
            angularVelocity
            (Argo.optional (Argo.fromString "angular_velocity") Argo.codec)

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
