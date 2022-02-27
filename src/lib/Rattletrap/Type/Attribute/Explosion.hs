module Rattletrap.Type.Attribute.Explosion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Explosion = Explosion
  { flag :: Bool
  , actorId :: I32.I32
  , location :: Vector.Vector
  }
  deriving (Eq, Show)

instance Argo.HasCodec Explosion where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ Explosion
      <$> Argo.project flag (Argo.required "flag" Argo.codec)
      <*> Argo.project actorId (Argo.required "actor_id" Argo.codec)
      <*> Argo.project location (Argo.required "location" Argo.codec)

bitPut :: Explosion -> BitPut.BitPut
bitPut explosionAttribute =
  BitPut.bool (flag explosionAttribute)
    <> I32.bitPut (actorId explosionAttribute)
    <> Vector.bitPut (location explosionAttribute)

bitGet :: Version.Version -> BitGet.BitGet Explosion
bitGet version = BitGet.label "Explosion" $ do
  flag <- BitGet.label "flag" BitGet.bool
  actorId <- BitGet.label "actorId" I32.bitGet
  location <- BitGet.label "location" $ Vector.bitGet version
  pure Explosion { flag, actorId, location }
