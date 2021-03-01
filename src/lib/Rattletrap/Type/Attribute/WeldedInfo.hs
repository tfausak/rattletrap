module Rattletrap.Type.Attribute.WeldedInfo where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data WeldedInfo = WeldedInfo
  { active :: Bool
  , actorId :: I32.I32
  , offset :: Vector.Vector
  , mass :: F32.F32
  , rotation :: Int8Vector.Int8Vector
  }
  deriving (Eq, Show)

instance Json.FromJSON WeldedInfo where
  parseJSON = Json.withObject "WeldedInfo" $ \object -> do
    active <- Json.required object "active"
    actorId <- Json.required object "actor_id"
    offset <- Json.required object "offset"
    mass <- Json.required object "mass"
    rotation <- Json.required object "rotation"
    pure WeldedInfo { active, actorId, offset, mass, rotation }

instance Json.ToJSON WeldedInfo where
  toJSON x = Json.object
    [ Json.pair "active" $ active x
    , Json.pair "actor_id" $ actorId x
    , Json.pair "offset" $ offset x
    , Json.pair "mass" $ mass x
    , Json.pair "rotation" $ rotation x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-welded-info" $ Schema.object
  [ (Json.pair "active" $ Schema.ref Schema.boolean, True)
  , (Json.pair "actor_id" $ Schema.ref I32.schema, True)
  , (Json.pair "offset" $ Schema.ref Vector.schema, True)
  , (Json.pair "mass" $ Schema.ref F32.schema, True)
  , (Json.pair "rotation" $ Schema.ref Int8Vector.schema, True)
  ]

bitPut :: WeldedInfo -> BitPut.BitPut
bitPut weldedInfoAttribute =
  BitPut.bool (active weldedInfoAttribute)
    <> I32.bitPut (actorId weldedInfoAttribute)
    <> Vector.bitPut (offset weldedInfoAttribute)
    <> F32.bitPut (mass weldedInfoAttribute)
    <> Int8Vector.bitPut (rotation weldedInfoAttribute)

bitGet :: Version.Version -> BitGet.BitGet WeldedInfo
bitGet version =
  WeldedInfo
    <$> BitGet.bool
    <*> I32.bitGet
    <*> Vector.bitGet version
    <*> F32.bitGet
    <*> Int8Vector.bitGet
