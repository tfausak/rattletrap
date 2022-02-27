module Rattletrap.Type.Attribute.Pickup where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data Pickup = Pickup
  { instigatorId :: Maybe U32.U32
  , pickedUp :: Bool
  }
  deriving (Eq, Show)

instance Argo.HasCodec Pickup where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Pickup
      <$> Argo.optional instigatorId "instigator_id"
      <*> Argo.required pickedUp "picked_up"

bitPut :: Pickup -> BitPut.BitPut
bitPut x =
  maybe
      (BitPut.bool False)
      (\y -> BitPut.bool True <> U32.bitPut y)
      (instigatorId x)
    <> BitPut.bool (pickedUp x)

bitGet :: BitGet.BitGet Pickup
bitGet = BitGet.label "Pickup" $ do
  instigator <- BitGet.label "instigator" BitGet.bool
  instigatorId <- BitGet.label "instigatorId"
    $ Monad.whenMaybe instigator U32.bitGet
  pickedUp <- BitGet.label "pickedUp" BitGet.bool
  pure Pickup { instigatorId, pickedUp }
