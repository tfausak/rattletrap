module Rattletrap.Type.Attribute.Explosion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Explosion = Explosion
  { flag :: Bool
  , actorId :: I32.I32
  , location :: Vector.Vector
  }
  deriving (Eq, Show)

instance Json.FromValue Explosion where
  fromValue = Json.withObject "Explosion" $ \object -> do
    flag <- Json.required object "flag"
    actorId <- Json.required object "actor_id"
    location <- Json.required object "location"
    pure Explosion { flag, actorId, location }

instance Json.ToValue Explosion where
  toValue x = Json.object
    [ Json.pair "flag" $ flag x
    , Json.pair "actor_id" $ actorId x
    , Json.pair "location" $ location x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-explosion" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "actor_id" $ Schema.ref I32.schema, True)
  , (Json.pair "location" $ Schema.ref Vector.schema, True)
  ]

bitPut :: Explosion -> BitPut.BitPut
bitPut explosionAttribute =
  BitPut.bool (flag explosionAttribute)
    <> I32.bitPut (actorId explosionAttribute)
    <> Vector.bitPut (location explosionAttribute)

bitGet :: Version.Version -> BitGet.BitGet Explosion
bitGet version = BitGet.label "Explosion" $ do
  flag <- BitGet.label "flag" BitGet.bool
  actorId <- BitGet.label "actorId" I32.bitGet
  location <- BitGet.label "location" $ Vector.bitGet version
  pure Explosion { flag, actorId, location }
