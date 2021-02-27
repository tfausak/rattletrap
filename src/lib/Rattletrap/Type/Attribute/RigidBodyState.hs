module Rattletrap.Type.Attribute.RigidBodyState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
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

schema :: Schema.Schema
schema = Schema.named "attribute-rigid-body-state" $ Schema.object
  [ (Json.pair "sleeping" $ Schema.json Schema.boolean, True)
  , (Json.pair "location" $ Schema.ref Vector.schema, True)
  , (Json.pair "rotation" $ Schema.ref Rotation.schema, True)
  , ( Json.pair "linear_velocity" . Schema.json $ Schema.maybe Vector.schema
    , False
    )
  , ( Json.pair "angular_velocity" . Schema.json $ Schema.maybe Vector.schema
    , False
    )
  ]

bitPut :: RigidBodyState -> BitPut.BitPut
bitPut rigidBodyStateAttribute =
  BitPut.bool (sleeping rigidBodyStateAttribute)
    <> Vector.bitPut (location rigidBodyStateAttribute)
    <> Rotation.bitPut (rotation rigidBodyStateAttribute)
    <> foldMap Vector.bitPut (linearVelocity rigidBodyStateAttribute)
    <> foldMap Vector.bitPut (angularVelocity rigidBodyStateAttribute)

bitGet :: Version.Version -> BitGet.BitGet RigidBodyState
bitGet version = do
  sleeping_ <- BitGet.bool
  RigidBodyState sleeping_
    <$> Vector.bitGet version
    <*> Rotation.bitGet version
    <*> whenMaybe (not sleeping_) (Vector.bitGet version)
    <*> whenMaybe (not sleeping_) (Vector.bitGet version)
