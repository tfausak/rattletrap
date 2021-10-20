module Rattletrap.Type.Attribute.PickupInfo where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data PickupInfo = PickupInfo
  { unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: U32.U32
  , unknown4 :: I32.I32
  , unknown5 :: I32.I32
  , unknown6 :: Bool
  , unknown7 :: Bool
  }
  deriving (Eq, Show)

instance Json.FromValue PickupInfo where
  fromValue = Json.withObject "PickupInfo" $ \object -> do
    unknown1 <- Json.required object "unknown1"
    unknown2 <- Json.required object "unknown2"
    unknown3 <- Json.required object "unknown3"
    unknown4 <- Json.required object "unknown4"
    unknown5 <- Json.required object "unknown5"
    unknown6 <- Json.required object "unknown6"
    unknown7 <- Json.required object "unknown7"
    pure PickupInfo
      { unknown1
      , unknown2
      , unknown3
      , unknown4
      , unknown5
      , unknown6
      , unknown7
      }

instance Json.ToValue PickupInfo where
  toValue x = Json.object
    [ Json.pair "unknown1" $ unknown1 x
    , Json.pair "unknown2" $ unknown2 x
    , Json.pair "unknown3" $ unknown3 x
    , Json.pair "unknown4" $ unknown4 x
    , Json.pair "unknown5" $ unknown5 x
    , Json.pair "unknown6" $ unknown6 x
    , Json.pair "unknown7" $ unknown7 x
    ]

schema :: Schema.Schema
schema = Schema.named "pickup-info" $ Schema.object
  [ (Json.pair "unknown1" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown2" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown3" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown4" $ Schema.ref I32.schema, True)
  , (Json.pair "unknown5" $ Schema.ref I32.schema, True)
  , (Json.pair "unknown6" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown7" $ Schema.ref Schema.boolean, True)
  ]

bitPut :: PickupInfo -> BitPut.BitPut
bitPut x =
  BitPut.bool (unknown1 x)
    <> BitPut.bool (unknown2 x)
    <> U32.bitPut (unknown3 x)
    <> I32.bitPut (unknown4 x)
    <> I32.bitPut (unknown5 x)
    <> BitPut.bool (unknown6 x)
    <> BitPut.bool (unknown7 x)

bitGet :: BitGet.BitGet PickupInfo
bitGet = BitGet.label "PickupInfo" $ do
  unknown1 <- BitGet.label "unknown1" BitGet.bool
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  unknown3 <- BitGet.label "unknown3" U32.bitGet
  unknown4 <- BitGet.label "unknown4" I32.bitGet
  unknown5 <- BitGet.label "unknown5" I32.bitGet
  unknown6 <- BitGet.label "unknown6" BitGet.bool
  unknown7 <- BitGet.label "unknown7" BitGet.bool
  pure PickupInfo
    { unknown1
    , unknown2
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    , unknown7
    }
