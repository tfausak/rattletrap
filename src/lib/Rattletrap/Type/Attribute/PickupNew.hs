module Rattletrap.Type.Attribute.PickupNew where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data PickupNew = PickupNew
  { instigatorId :: Maybe U32.U32
  , pickedUp :: U8.U8
  }
  deriving (Eq, Show)

instance Argo.HasCodec PickupNew where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ PickupNew
      <$> Argo.optional instigatorId "instigator_id"
      <*> Argo.required pickedUp "picked_up"

bitPut :: PickupNew -> BitPut.BitPut
bitPut x =
  maybe
      (BitPut.bool False)
      (\y -> BitPut.bool True <> U32.bitPut y)
      (instigatorId x)
    <> U8.bitPut (pickedUp x)

bitGet :: BitGet.BitGet PickupNew
bitGet = BitGet.label "PickupNew" $ do
  instigator <- BitGet.label "instigator" BitGet.bool
  instigatorId <- BitGet.label "instigatorId"
    $ Monad.whenMaybe instigator U32.bitGet
  pickedUp <- BitGet.label "pickedUp" U8.bitGet
  pure PickupNew { instigatorId, pickedUp }
