module Rattletrap.Type.ObjectTarget where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json

data ObjectTarget = ObjectTarget
  { isActor :: Bool,
    targetIndex :: I32.I32
  }
  deriving (Eq, Show)

instance Json.FromJSON ObjectTarget where
  parseJSON = Json.withObject "ObjectTarget" $ \object -> do
    isActor <- Json.required object "is_actor"
    targetIndex <- Json.required object "target_index"
    pure
      ObjectTarget
        { isActor,
          targetIndex
        }

instance Json.ToJSON ObjectTarget where
  toJSON x =
    Json.object
      [ Json.pair "is_actor" $ isActor x,
        Json.pair "target_index" $ targetIndex x
      ]

schema :: Schema.Schema
schema =
  Schema.named "object-target" $
    Schema.object
      [ (Json.pair "is_actor" $ Schema.ref Schema.boolean, True),
        (Json.pair "target_index" $ Schema.ref I32.schema, True)
      ]

bitPut :: ObjectTarget -> BitPut.BitPut
bitPut demolishAttribute =
  BitPut.bool (isActor demolishAttribute)
    <> I32.bitPut (targetIndex demolishAttribute)

bitGet :: BitGet.BitGet ObjectTarget
bitGet = BitGet.label "ObjectTarget" $ do
  isActor <- BitGet.label "isActor" BitGet.bool
  targetIndex <- BitGet.label "targetIndex" I32.bitGet
  pure
    ObjectTarget
      { isActor,
        targetIndex
      }
