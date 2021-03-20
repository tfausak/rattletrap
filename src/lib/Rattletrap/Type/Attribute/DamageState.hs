module Rattletrap.Type.Attribute.DamageState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data DamageState = DamageState
  { unknown1 :: U8.U8
  , unknown2 :: Bool
  , unknown3 :: I32.I32
  , unknown4 :: Vector.Vector
  , unknown5 :: Bool
  , unknown6 :: Bool
  }
  deriving (Eq, Show)

instance Json.FromJSON DamageState where
  parseJSON = Json.withObject "DamageState" $ \object -> do
    unknown1 <- Json.required object "unknown1"
    unknown2 <- Json.required object "unknown2"
    unknown3 <- Json.required object "unknown3"
    unknown4 <- Json.required object "unknown4"
    unknown5 <- Json.required object "unknown5"
    unknown6 <- Json.required object "unknown6"
    pure DamageState
      { unknown1
      , unknown2
      , unknown3
      , unknown4
      , unknown5
      , unknown6
      }

instance Json.ToJSON DamageState where
  toJSON x = Json.object
    [ Json.pair "unknown1" $ unknown1 x
    , Json.pair "unknown2" $ unknown2 x
    , Json.pair "unknown3" $ unknown3 x
    , Json.pair "unknown4" $ unknown4 x
    , Json.pair "unknown5" $ unknown5 x
    , Json.pair "unknown6" $ unknown6 x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-damage-state" $ Schema.object
  [ (Json.pair "unknown1" $ Schema.ref U8.schema, True)
  , (Json.pair "unknown2" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown3" $ Schema.ref I32.schema, True)
  , (Json.pair "unknown4" $ Schema.ref Vector.schema, True)
  , (Json.pair "unknown5" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown6" $ Schema.ref Schema.boolean, True)
  ]

bitPut :: DamageState -> BitPut.BitPut
bitPut damageStateAttribute =
  U8.bitPut (unknown1 damageStateAttribute)
    <> BitPut.bool (unknown2 damageStateAttribute)
    <> I32.bitPut (unknown3 damageStateAttribute)
    <> Vector.bitPut (unknown4 damageStateAttribute)
    <> BitPut.bool (unknown5 damageStateAttribute)
    <> BitPut.bool (unknown6 damageStateAttribute)

bitGet :: Version.Version -> BitGet.BitGet DamageState
bitGet version = BitGet.label "CustomDemolish" $ do
  unknown1 <- BitGet.label "unknown1" U8.bitGet
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  unknown3 <- BitGet.label "unknown3" I32.bitGet
  unknown4 <- BitGet.label "unknown4" $ Vector.bitGet version
  unknown5 <- BitGet.label "unknown5" BitGet.bool
  unknown6 <- BitGet.label "unknown6" BitGet.bool
  pure DamageState
    { unknown1
    , unknown2
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    }
