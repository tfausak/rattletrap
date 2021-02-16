module Rattletrap.Type.Attribute.RigidBodyState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Utility.Monad

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
bitPut rigidBodyStateAttribute =
  BitPut.bool (sleeping rigidBodyStateAttribute)
    <> Vector.bitPut (location rigidBodyStateAttribute)
    <> Rotation.bitPut (rotation rigidBodyStateAttribute)
    <> foldMap Vector.bitPut (linearVelocity rigidBodyStateAttribute)
    <> foldMap Vector.bitPut (angularVelocity rigidBodyStateAttribute)

bitGet :: (Int, Int, Int) -> BitGet.BitGet RigidBodyState
bitGet version = do
  sleeping_ <- BitGet.bool
  RigidBodyState sleeping_
    <$> Vector.bitGet version
    <*> Rotation.bitGet version
    <*> whenMaybe (not sleeping_) (Vector.bitGet version)
    <*> whenMaybe (not sleeping_) (Vector.bitGet version)
