module Rattletrap.Type.Attribute.AppliedDamage where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data AppliedDamage = AppliedDamage
  { unknown1 :: U8.U8
  , location :: Vector.Vector
  , unknown3 :: I32.I32
  , unknown4 :: I32.I32
  }
  deriving (Eq, Show)

instance Argo.HasCodec AppliedDamage where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ AppliedDamage
      <$> Argo.project unknown1 (Argo.required "unknown1" Argo.codec)
      <*> Argo.project location (Argo.required "location" Argo.codec)
      <*> Argo.project unknown3 (Argo.required "unknown3" Argo.codec)
      <*> Argo.project unknown4 (Argo.required "unknown4" Argo.codec)

bitPut :: AppliedDamage -> BitPut.BitPut
bitPut appliedDamageAttribute =
  U8.bitPut (unknown1 appliedDamageAttribute)
    <> Vector.bitPut (location appliedDamageAttribute)
    <> I32.bitPut (unknown3 appliedDamageAttribute)
    <> I32.bitPut (unknown4 appliedDamageAttribute)

bitGet :: Version.Version -> BitGet.BitGet AppliedDamage
bitGet version = BitGet.label "AppliedDamage" $ do
  unknown1 <- BitGet.label "unknown1" U8.bitGet
  location <- BitGet.label "location" $ Vector.bitGet version
  unknown3 <- BitGet.label "unknown3" I32.bitGet
  unknown4 <- BitGet.label "unknown4" I32.bitGet
  pure AppliedDamage { unknown1, location, unknown3, unknown4 }
