module Rattletrap.Type.Attribute.FlaggedByte where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data FlaggedByte = FlaggedByte
  { flag :: Bool
  , byte :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedByte)

schema :: Schema.Schema
schema = Schema.named "attribute-flagged-byte" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "byte" $ Schema.ref U8.schema, True)
  ]

bitPut :: FlaggedByte -> BitPut.BitPut
bitPut flaggedByteAttribute = BitPut.bool (flag flaggedByteAttribute)
  <> U8.bitPut (byte flaggedByteAttribute)

bitGet :: BitGet.BitGet FlaggedByte
bitGet = FlaggedByte <$> BitGet.bool <*> U8.bitGet
