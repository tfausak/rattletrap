module Rattletrap.Type.Attribute.CustomDemolish where

import Prelude hiding (id)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data CustomDemolish = CustomDemolish
  { flag :: Bool
  , id :: I32.I32
  , demolish :: Demolish.Demolish
  }
  deriving (Eq, Show)

instance Argo.HasCodec CustomDemolish where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ CustomDemolish
      <$> Argo.required flag "flag"
      <*> Argo.required id "id"
      <*> Argo.required demolish "demolish"

bitPut :: CustomDemolish -> BitPut.BitPut
bitPut x =
  BitPut.bool (flag x)
    <> I32.bitPut (Rattletrap.Type.Attribute.CustomDemolish.id x)
    <> Demolish.bitPut (demolish x)

bitGet :: Version.Version -> BitGet.BitGet CustomDemolish
bitGet version = BitGet.label "CustomDemolish" $ do
  flag <- BitGet.label "flag" BitGet.bool
  id <- BitGet.label "id" I32.bitGet
  demolish <- BitGet.label "demolish" $ Demolish.bitGet version
  pure CustomDemolish
    { flag
    , Rattletrap.Type.Attribute.CustomDemolish.id
    , demolish
    }
