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
    Argo.fromObjectCodec Argo.Allow
      $ Pickup
      <$> Argo.project
            instigatorId
            (Argo.optional (Argo.fromString "instigator_id") Argo.codec)
      <*> Argo.project
            pickedUp
            (Argo.required (Argo.fromString "picked_up") Argo.codec)

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
