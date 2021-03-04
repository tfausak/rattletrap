module Rattletrap.Type.Attribute.AppliedDamage where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data AppliedDamage = AppliedDamage
  { unknown1 :: U8.U8
  , location :: Vector.Vector
  , unknown3 :: I32.I32
  , unknown4 :: I32.I32
  }
  deriving (Eq, Show)

instance Json.FromJSON AppliedDamage where
  parseJSON = Json.withObject "AppliedDamage" $ \object -> do
    unknown1 <- Json.required object "unknown1"
    location <- Json.required object "location"
    unknown3 <- Json.required object "unknown3"
    unknown4 <- Json.required object "unknown4"
    pure AppliedDamage { unknown1, location, unknown3, unknown4 }

instance Json.ToJSON AppliedDamage where
  toJSON x = Json.object
    [ Json.pair "unknown1" $ unknown1 x
    , Json.pair "location" $ location x
    , Json.pair "unknown3" $ unknown3 x
    , Json.pair "unknown4" $ unknown4 x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-applied-damage" $ Schema.object
  [ (Json.pair "unknown1" $ Schema.ref U8.schema, True)
  , (Json.pair "location" $ Schema.ref Vector.schema, True)
  , (Json.pair "unknown3" $ Schema.ref I32.schema, True)
  , (Json.pair "unknown4" $ Schema.ref I32.schema, True)
  ]

bitPut :: AppliedDamage -> BitPut.BitPut
bitPut appliedDamageAttribute =
  U8.bitPut (unknown1 appliedDamageAttribute)
    <> Vector.bitPut (location appliedDamageAttribute)
    <> I32.bitPut (unknown3 appliedDamageAttribute)
    <> I32.bitPut (unknown4 appliedDamageAttribute)

bitGet :: Version.Version -> BitGet.BitGet AppliedDamage
bitGet version = do
  unknown1 <- U8.bitGet
  location <- Vector.bitGet version
  unknown3 <- I32.bitGet
  unknown4 <- I32.bitGet
  pure AppliedDamage { unknown1, location, unknown3, unknown4 }
