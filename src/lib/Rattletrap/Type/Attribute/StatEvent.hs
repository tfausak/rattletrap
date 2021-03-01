module Rattletrap.Type.Attribute.StatEvent where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json

data StatEvent = StatEvent
  { unknown :: Bool
  , objectId :: I32.I32
  }
  deriving (Eq, Show)

instance Json.FromJSON StatEvent where
  parseJSON = Json.withObject "StatEvent" $ \object -> do
    unknown <- Json.required object "unknown"
    objectId <- Json.required object "object_id"
    pure StatEvent { unknown, objectId }

instance Json.ToJSON StatEvent where
  toJSON x = Json.object
    [Json.pair "unknown" $ unknown x, Json.pair "object_id" $ objectId x]

schema :: Schema.Schema
schema = Schema.named "attribute-stat-event" $ Schema.object
  [ (Json.pair "unknown" $ Schema.ref Schema.boolean, True)
  , (Json.pair "object_id" $ Schema.ref I32.schema, True)
  ]

bitPut :: StatEvent -> BitPut.BitPut
bitPut statEventAttribute = BitPut.bool (unknown statEventAttribute)
  <> I32.bitPut (objectId statEventAttribute)

bitGet :: BitGet.BitGet StatEvent
bitGet = StatEvent <$> BitGet.bool <*> I32.bitGet
