module Rattletrap.Type.Replication.Destroyed where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Vendor.Argo as Argo

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data Destroyed = Destroyed
  deriving (Eq, Show)

instance Argo.HasCodec Destroyed where
  codec = Argo.identified $ Argo.map (const Destroyed) (const ()) Argo.codec

bitPut :: Destroyed -> BitPut.BitPut
bitPut _ = mempty

bitGet :: BitGet.BitGet Destroyed
bitGet = BitGet.label "Destroyed" $ pure Destroyed
