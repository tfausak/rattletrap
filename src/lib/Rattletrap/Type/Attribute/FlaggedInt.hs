module Rattletrap.Type.Attribute.FlaggedInt where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Utility.Json as Json

data FlaggedInt = FlaggedInt
  { flag :: Bool
  , int :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedInt)

schema :: Schema.Schema
schema = Schema.named "attribute-flagged-int" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "int" $ Schema.ref I32.schema, True)
  ]

bitPut :: FlaggedInt -> BitPut.BitPut
bitPut flaggedIntAttribute = BitPut.bool (flag flaggedIntAttribute)
  <> I32.bitPut (int flaggedIntAttribute)

bitGet :: BitGet.BitGet FlaggedInt
bitGet = FlaggedInt <$> BitGet.bool <*> I32.bitGet
