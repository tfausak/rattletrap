module Rattletrap.Type.Attribute.Title where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
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

$(deriveJson ''Title)

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
bitGet =
  Title
    <$> BitGet.bool
    <*> BitGet.bool
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> BitGet.bool
