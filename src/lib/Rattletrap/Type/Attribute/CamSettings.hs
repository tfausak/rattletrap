module Rattletrap.Type.Attribute.CamSettings where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data CamSettings = CamSettings
  { fov :: F32.F32
  , height :: F32.F32
  , angle :: F32.F32
  , distance :: F32.F32
  , stiffness :: F32.F32
  , swivelSpeed :: F32.F32
  , transitionSpeed :: Maybe F32.F32
  }
  deriving (Eq, Show)

instance Json.FromJSON CamSettings where
  parseJSON = Json.withObject "CamSettings" $ \object -> do
    fov <- Json.required object "fov"
    height <- Json.required object "height"
    angle <- Json.required object "angle"
    distance <- Json.required object "distance"
    stiffness <- Json.required object "stiffness"
    swivelSpeed <- Json.required object "swivel_speed"
    transitionSpeed <- Json.optional object "transition_speed"
    pure CamSettings
      { fov
      , height
      , angle
      , distance
      , stiffness
      , swivelSpeed
      , transitionSpeed
      }

instance Json.ToJSON CamSettings where
  toJSON x = Json.object
    [ Json.pair "fov" $ fov x
    , Json.pair "height" $ height x
    , Json.pair "angle" $ angle x
    , Json.pair "distance" $ distance x
    , Json.pair "stiffness" $ stiffness x
    , Json.pair "swivel_speed" $ swivelSpeed x
    , Json.pair "transition_speed" $ transitionSpeed x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-cam-settings" $ Schema.object
  [ (Json.pair "fov" $ Schema.ref F32.schema, True)
  , (Json.pair "height" $ Schema.ref F32.schema, True)
  , (Json.pair "angle" $ Schema.ref F32.schema, True)
  , (Json.pair "distance" $ Schema.ref F32.schema, True)
  , (Json.pair "stiffness" $ Schema.ref F32.schema, True)
  , (Json.pair "swivel_speed" $ Schema.ref F32.schema, True)
  , ( Json.pair "transition_speed" . Schema.json $ Schema.maybe F32.schema
    , False
    )
  ]

bitPut :: CamSettings -> BitPut.BitPut
bitPut camSettingsAttribute =
  F32.bitPut (fov camSettingsAttribute)
    <> F32.bitPut (height camSettingsAttribute)
    <> F32.bitPut (angle camSettingsAttribute)
    <> F32.bitPut (distance camSettingsAttribute)
    <> F32.bitPut (stiffness camSettingsAttribute)
    <> F32.bitPut (swivelSpeed camSettingsAttribute)
    <> foldMap F32.bitPut (transitionSpeed camSettingsAttribute)

bitGet :: Version.Version -> BitGet.BitGet CamSettings
bitGet version = do
  fov <- F32.bitGet
  height <- F32.bitGet
  angle <- F32.bitGet
  distance <- F32.bitGet
  stiffness <- F32.bitGet
  swivelSpeed <- F32.bitGet
  transitionSpeed <- Monad.whenMaybe
    (Version.atLeast 868 20 0 version)
    F32.bitGet
  pure CamSettings
    { fov
    , height
    , angle
    , distance
    , stiffness
    , swivelSpeed
    , transitionSpeed
    }
