module Rattletrap.Type.Attribute.Explosion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
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

$(deriveJson ''Explosion)

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
bitGet version =
  Explosion <$> BitGet.bool <*> I32.bitGet <*> Vector.bitGet version
