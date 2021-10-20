module Rattletrap.Type.Attribute.FlaggedInt where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json

data FlaggedInt = FlaggedInt
  { flag :: Bool
  , int :: I32.I32
  }
  deriving (Eq, Show)

instance Json.FromValue FlaggedInt where
  fromValue = Json.withObject "FlaggedInt" $ \object -> do
    flag <- Json.required object "flag"
    int <- Json.required object "int"
    pure FlaggedInt { flag, int }

instance Json.ToValue FlaggedInt where
  toValue x = Json.object [Json.pair "flag" $ flag x, Json.pair "int" $ int x]

schema :: Schema.Schema
schema = Schema.named "attribute-flagged-int" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "int" $ Schema.ref I32.schema, True)
  ]

bitPut :: FlaggedInt -> BitPut.BitPut
bitPut flaggedIntAttribute = BitPut.bool (flag flaggedIntAttribute)
  <> I32.bitPut (int flaggedIntAttribute)

bitGet :: BitGet.BitGet FlaggedInt
bitGet = BitGet.label "FlaggedInt" $ do
  flag <- BitGet.label "flag" BitGet.bool
  int <- BitGet.label "int" I32.bitGet
  pure FlaggedInt { flag, int }
