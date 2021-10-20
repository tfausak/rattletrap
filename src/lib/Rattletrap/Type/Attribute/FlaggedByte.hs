module Rattletrap.Type.Attribute.FlaggedByte where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

data FlaggedByte = FlaggedByte
  { flag :: Bool
  , byte :: U8.U8
  }
  deriving (Eq, Show)

instance Json.FromValue FlaggedByte where
  fromValue = Json.withObject "FlaggedByte" $ \object -> do
    flag <- Json.required object "flag"
    byte <- Json.required object "byte"
    pure FlaggedByte { flag, byte }

instance Json.ToValue FlaggedByte where
  toValue x =
    Json.object [Json.pair "flag" $ flag x, Json.pair "byte" $ byte x]

schema :: Schema.Schema
schema = Schema.named "attribute-flagged-byte" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "byte" $ Schema.ref U8.schema, True)
  ]

bitPut :: FlaggedByte -> BitPut.BitPut
bitPut flaggedByteAttribute = BitPut.bool (flag flaggedByteAttribute)
  <> U8.bitPut (byte flaggedByteAttribute)

bitGet :: BitGet.BitGet FlaggedByte
bitGet = BitGet.label "FlaggedByte" $ do
  flag <- BitGet.label "flag" BitGet.bool
  byte <- BitGet.label "byte" U8.bitGet
  pure FlaggedByte { flag, byte }
