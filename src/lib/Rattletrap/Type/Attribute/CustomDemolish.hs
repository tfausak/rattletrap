module Rattletrap.Type.Attribute.CustomDemolish where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data CustomDemolish = CustomDemolish
  { flag :: Bool,
    id :: I32.I32,
    demolish :: Demolish.Demolish
  }
  deriving (Eq, Show)

instance Json.FromJSON CustomDemolish where
  parseJSON = Json.withObject "CustomDemolish" $ \object -> do
    flag <- Json.required object "flag"
    id_ <- Json.required object "id"
    demolish <- Json.required object "demolish"
    pure CustomDemolish {flag, Rattletrap.Type.Attribute.CustomDemolish.id = id_, demolish}

instance Json.ToJSON CustomDemolish where
  toJSON x =
    Json.object
      [ Json.pair "flag" $ flag x,
        Json.pair "id" $ Rattletrap.Type.Attribute.CustomDemolish.id x,
        Json.pair "demolish" $ demolish x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute-custom-demolish" $
    Schema.object
      [ (Json.pair "flag" $ Schema.ref Schema.boolean, True),
        (Json.pair "id" $ Schema.ref I32.schema, True),
        (Json.pair "demolish" $ Schema.ref Demolish.schema, True)
      ]

bitPut :: CustomDemolish -> BitPut.BitPut
bitPut x =
  BitPut.bool (flag x)
    <> I32.bitPut (Rattletrap.Type.Attribute.CustomDemolish.id x)
    <> Demolish.bitPut (demolish x)

bitGet :: Version.Version -> BitGet.BitGet CustomDemolish
bitGet version = BitGet.label "CustomDemolish" $ do
  flag <- BitGet.label "flag" BitGet.bool
  id_ <- BitGet.label "id" I32.bitGet
  demolish <- BitGet.label "demolish" $ Demolish.bitGet version
  pure
    CustomDemolish
      { flag,
        Rattletrap.Type.Attribute.CustomDemolish.id = id_,
        demolish
      }
