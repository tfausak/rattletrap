module Rattletrap.Type.Attribute.Boost where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

data Boost = Boost
  { grantCount :: U8.U8,
    boostAmount :: U8.U8,
    unused1 :: U8.U8,
    unused2 :: U8.U8
  }
  deriving (Eq, Show)

instance Json.FromJSON Boost where
  parseJSON = Json.withObject "Boost" $ \object -> do
    grantCount <- Json.required object "grantCount"
    boostAmount <- Json.required object "boostAmount"
    unused1 <- Json.required object "unused1"
    unused2 <- Json.required object "unused2"
    pure Boost {grantCount, boostAmount, unused1, unused2}

instance Json.ToJSON Boost where
  toJSON x =
    Json.object
      [ Json.pair "grantCount" $ grantCount x,
        Json.pair "boostAmount" $ boostAmount x,
        Json.pair "unused1" $ unused1 x,
        Json.pair "unused2" $ unused2 x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute-applied-damage" $
    Schema.object
      [ (Json.pair "grantCount" $ Schema.ref U8.schema, True),
        (Json.pair "boostAmount" $ Schema.ref U8.schema, True),
        (Json.pair "unused1" $ Schema.ref U8.schema, True),
        (Json.pair "unused2" $ Schema.ref U8.schema, True)
      ]

bitPut :: Boost -> BitPut.BitPut
bitPut appliedDamageAttribute =
  U8.bitPut (grantCount appliedDamageAttribute)
    <> U8.bitPut (boostAmount appliedDamageAttribute)
    <> U8.bitPut (unused1 appliedDamageAttribute)
    <> U8.bitPut (unused2 appliedDamageAttribute)

bitGet :: BitGet.BitGet Boost
bitGet = BitGet.label "Boost" $ do
  grantCount <- BitGet.label "grantCount" U8.bitGet
  boostAmount <- BitGet.label "boostAmount" U8.bitGet
  unused1 <- BitGet.label "unused1" U8.bitGet
  unused2 <- BitGet.label "unused2" U8.bitGet
  pure Boost {grantCount, boostAmount, unused1, unused2}
