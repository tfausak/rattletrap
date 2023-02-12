module Rattletrap.Type.RemoteId.Switch where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

data Switch = Switch
  { a :: U64.U64,
    b :: U64.U64,
    c :: U64.U64,
    d :: U64.U64
  }
  deriving (Eq, Show)

instance Json.FromJSON Switch where
  parseJSON json = do
    (a, b, c, d) <- Json.parseJSON json
    pure Switch {a, b, c, d}

instance Json.ToJSON Switch where
  toJSON x = Json.toJSON (a x, b x, c x, d x)

schema :: Schema.Schema
schema =
  Schema.named "remote-id-switch" . Schema.tuple . replicate 4 $
    Schema.ref
      U64.schema

bitPut :: Switch -> BitPut.BitPut
bitPut x =
  U64.bitPut (a x) <> U64.bitPut (b x) <> U64.bitPut (c x) <> U64.bitPut (d x)

bitGet :: BitGet.BitGet Switch
bitGet = BitGet.label "Switch" $ do
  a <- U64.bitGet
  b <- U64.bitGet
  c <- U64.bitGet
  d <- U64.bitGet
  pure Switch {a, b, c, d}
