module Rattletrap.Type.Attribute.DamageState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data DamageState = DamageState
  { unknown1 :: U8.U8
  , unknown2 :: Bool
  , unknown3 :: I32.I32
  , unknown4 :: Vector.Vector
  , unknown5 :: Bool
  , unknown6 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''DamageState)

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
bitGet version =
  DamageState
    <$> U8.bitGet
    <*> BitGet.bool
    <*> I32.bitGet
    <*> Vector.bitGet version
    <*> BitGet.bool
    <*> BitGet.bool
