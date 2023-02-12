module Rattletrap.Type.Attribute.RepStatTitle where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data RepStatTitle = RepStatTitle
  { unknown :: Bool,
    name :: Str.Str,
    target :: FlaggedInt.FlaggedInt,
    value :: U32.U32
  }
  deriving (Eq, Show)

instance Json.FromJSON RepStatTitle where
  parseJSON = Json.withObject "RepStatTitle" $ \object -> do
    unknown <- Json.required object "unknown"
    name <- Json.required object "name"
    target <- Json.required object "target"
    value <- Json.required object "value"
    pure RepStatTitle {unknown, name, target, value}

instance Json.ToJSON RepStatTitle where
  toJSON x =
    Json.object
      [ Json.pair "unknown" $ unknown x,
        Json.pair "name" $ name x,
        Json.pair "target" $ target x,
        Json.pair "value" $ value x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute-rep-stat-title" $
    Schema.object
      [ (Json.pair "unknown" $ Schema.ref Schema.boolean, True),
        (Json.pair "name" $ Schema.ref Str.schema, True),
        (Json.pair "target" $ Schema.ref FlaggedInt.schema, True),
        (Json.pair "value" $ Schema.ref U32.schema, True)
      ]

bitPut :: RepStatTitle -> BitPut.BitPut
bitPut x =
  BitPut.bool (unknown x)
    <> Str.bitPut (name x)
    <> FlaggedInt.bitPut (target x)
    <> U32.bitPut (value x)

bitGet :: BitGet.BitGet RepStatTitle
bitGet = BitGet.label "RepStatTitle" $ do
  unknown <- BitGet.label "unknown" BitGet.bool
  name <- BitGet.label "name" Str.bitGet
  target <- BitGet.label "target" FlaggedInt.bitGet
  value <- BitGet.label "value" U32.bitGet
  pure RepStatTitle {unknown, name, target, value}
