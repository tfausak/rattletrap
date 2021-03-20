module Rattletrap.Type.Attribute.RigidBodyState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data RigidBodyState = RigidBodyState
  { sleeping :: Bool
  , location :: Vector.Vector
  , rotation :: Rotation.Rotation
  , linearVelocity :: Maybe Vector.Vector
  , angularVelocity :: Maybe Vector.Vector
  }
  deriving (Eq, Show)

instance Json.FromJSON RigidBodyState where
  parseJSON = Json.withObject "RigidBodyState" $ \object -> do
    sleeping <- Json.required object "sleeping"
    location <- Json.required object "location"
    rotation <- Json.required object "rotation"
    linearVelocity <- Json.optional object "linear_velocity"
    angularVelocity <- Json.optional object "angular_velocity"
    pure RigidBodyState
      { sleeping
      , location
      , rotation
      , linearVelocity
      , angularVelocity
      }

instance Json.ToJSON RigidBodyState where
  toJSON x = Json.object
    [ Json.pair "sleeping" $ sleeping x
    , Json.pair "location" $ location x
    , Json.pair "rotation" $ rotation x
    , Json.pair "linear_velocity" $ linearVelocity x
    , Json.pair "angular_velocity" $ angularVelocity x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-rigid-body-state" $ Schema.object
  [ (Json.pair "sleeping" $ Schema.ref Schema.boolean, True)
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
