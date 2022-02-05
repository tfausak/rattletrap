module Rattletrap.Type.Attribute.WeldedInfo where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data WeldedInfo = WeldedInfo
  { active :: Bool
  , actorId :: I32.I32
  , offset :: Vector.Vector
  , mass :: F32.F32
  , rotation :: Int8Vector.Int8Vector
  }
  deriving (Eq, Show)

instance Argo.HasCodec WeldedInfo where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ WeldedInfo
      <$> Argo.project
            active
            (Argo.required (Argo.fromString "active") Argo.codec)
      <*> Argo.project
            actorId
            (Argo.required (Argo.fromString "actor_id") Argo.codec)
      <*> Argo.project
            offset
            (Argo.required (Argo.fromString "offset") Argo.codec)
      <*> Argo.project mass (Argo.required (Argo.fromString "mass") Argo.codec)
      <*> Argo.project
            rotation
            (Argo.required (Argo.fromString "rotation") Argo.codec)

bitPut :: WeldedInfo -> BitPut.BitPut
bitPut weldedInfoAttribute =
  BitPut.bool (active weldedInfoAttribute)
    <> I32.bitPut (actorId weldedInfoAttribute)
    <> Vector.bitPut (offset weldedInfoAttribute)
    <> F32.bitPut (mass weldedInfoAttribute)
    <> Int8Vector.bitPut (rotation weldedInfoAttribute)

bitGet :: Version.Version -> BitGet.BitGet WeldedInfo
bitGet version = BitGet.label "WeldedInfo" $ do
  active <- BitGet.label "active" BitGet.bool
  actorId <- BitGet.label "actorId" I32.bitGet
  offset <- BitGet.label "offset" $ Vector.bitGet version
  mass <- BitGet.label "mass" F32.bitGet
  rotation <- BitGet.label "rotation" Int8Vector.bitGet
  pure WeldedInfo { active, actorId, offset, mass, rotation }
