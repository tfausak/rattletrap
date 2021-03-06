module Rattletrap.Type.Attribute.Title where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data Title = Title
  { unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: U32.U32
  , unknown4 :: U32.U32
  , unknown5 :: U32.U32
  , unknown6 :: U32.U32
  , unknown7 :: U32.U32
  , unknown8 :: Bool
  }
  deriving (Eq, Show)

instance Json.FromJSON Title where
  parseJSON = Json.withObject "Title" $ \object -> do
    unknown1 <- Json.required object "unknown1"
    unknown2 <- Json.required object "unknown2"
    unknown3 <- Json.required object "unknown3"
    unknown4 <- Json.required object "unknown4"
    unknown5 <- Json.required object "unknown5"
    unknown6 <- Json.required object "unknown6"
    unknown7 <- Json.required object "unknown7"
    unknown8 <- Json.required object "unknown8"
    pure Title
      { unknown1
      , unknown2
      , unknown3
      , unknown4
      , unknown5
      , unknown6
      , unknown7
      , unknown8
      }

instance Json.ToJSON Title where
  toJSON x = Json.object
    [ Json.pair "unknown1" $ unknown1 x
    , Json.pair "unknown2" $ unknown2 x
    , Json.pair "unknown3" $ unknown3 x
    , Json.pair "unknown4" $ unknown4 x
    , Json.pair "unknown5" $ unknown5 x
    , Json.pair "unknown6" $ unknown6 x
    , Json.pair "unknown7" $ unknown7 x
    , Json.pair "unknown8" $ unknown8 x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-title" $ Schema.object
  [ (Json.pair "unknown1" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown2" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown3" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown4" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown5" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown6" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown7" $ Schema.ref U32.schema, True)
  , (Json.pair "unknown8" $ Schema.ref Schema.boolean, True)
  ]

bitPut :: Title -> BitPut.BitPut
bitPut titleAttribute =
  BitPut.bool (unknown1 titleAttribute)
    <> BitPut.bool (unknown2 titleAttribute)
    <> U32.bitPut (unknown3 titleAttribute)
    <> U32.bitPut (unknown4 titleAttribute)
    <> U32.bitPut (unknown5 titleAttribute)
    <> U32.bitPut (unknown6 titleAttribute)
    <> U32.bitPut (unknown7 titleAttribute)
    <> BitPut.bool (unknown8 titleAttribute)

bitGet :: BitGet.BitGet Title
bitGet = do
  unknown1 <- BitGet.bool
  unknown2 <- BitGet.bool
  unknown3 <- U32.bitGet
  unknown4 <- U32.bitGet
  unknown5 <- U32.bitGet
  unknown6 <- U32.bitGet
  unknown7 <- U32.bitGet
  unknown8 <- BitGet.bool
  pure Title
    { unknown1
    , unknown2
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    , unknown7
    , unknown8
    }
