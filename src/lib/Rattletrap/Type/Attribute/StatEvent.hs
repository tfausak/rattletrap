module Rattletrap.Type.Attribute.StatEvent where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data StatEvent = StatEvent
  { unknown :: Bool
  , objectId :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''StatEvent)

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
