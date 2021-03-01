module Rattletrap.Type.Attribute.CustomDemolish where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data CustomDemolish = CustomDemolish
  { flag :: Bool
  , id :: I32.I32
  , demolish :: Demolish.Demolish
  }
  deriving (Eq, Show)

$(deriveJson ''CustomDemolish)

schema :: Schema.Schema
schema = Schema.named "attribute-custom-demolish" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "id" $ Schema.ref I32.schema, True)
  , (Json.pair "demolish" $ Schema.ref Demolish.schema, True)
  ]

bitPut :: CustomDemolish -> BitPut.BitPut
bitPut x =
  BitPut.bool (flag x)
    <> I32.bitPut (Rattletrap.Type.Attribute.CustomDemolish.id x)
    <> Demolish.bitPut (demolish x)

bitGet :: Version.Version -> BitGet.BitGet CustomDemolish
bitGet version =
  CustomDemolish <$> BitGet.bool <*> I32.bitGet <*> Demolish.bitGet version
