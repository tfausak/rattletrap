module Rattletrap.Type.Attribute.DamageState where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data DamageState = DamageState
  { unknown1 :: U8.U8
  , unknown2 :: Bool
  , unknown3 :: I32.I32
  , unknown4 :: Vector.Vector
  , unknown5 :: Bool
  , unknown6 :: Bool
  }
  deriving (Eq, Show)

instance Argo.HasCodec DamageState where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ DamageState
      <$> Argo.project
            unknown1
            (Argo.required (Argo.fromString "unknown1") Argo.codec)
      <*> Argo.project
            unknown2
            (Argo.required (Argo.fromString "unknown2") Argo.codec)
      <*> Argo.project
            unknown3
            (Argo.required (Argo.fromString "unknown3") Argo.codec)
      <*> Argo.project
            unknown4
            (Argo.required (Argo.fromString "unknown4") Argo.codec)
      <*> Argo.project
            unknown5
            (Argo.required (Argo.fromString "unknown5") Argo.codec)
      <*> Argo.project
            unknown6
            (Argo.required (Argo.fromString "unknown6") Argo.codec)

bitPut :: DamageState -> BitPut.BitPut
bitPut damageStateAttribute =
  U8.bitPut (unknown1 damageStateAttribute)
    <> BitPut.bool (unknown2 damageStateAttribute)
    <> I32.bitPut (unknown3 damageStateAttribute)
    <> Vector.bitPut (unknown4 damageStateAttribute)
    <> BitPut.bool (unknown5 damageStateAttribute)
    <> BitPut.bool (unknown6 damageStateAttribute)

bitGet :: Version.Version -> BitGet.BitGet DamageState
bitGet version = BitGet.label "CustomDemolish" $ do
  unknown1 <- BitGet.label "unknown1" U8.bitGet
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  unknown3 <- BitGet.label "unknown3" I32.bitGet
  unknown4 <- BitGet.label "unknown4" $ Vector.bitGet version
  unknown5 <- BitGet.label "unknown5" BitGet.bool
  unknown6 <- BitGet.label "unknown6" BitGet.bool
  pure DamageState
    { unknown1
    , unknown2
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    }
