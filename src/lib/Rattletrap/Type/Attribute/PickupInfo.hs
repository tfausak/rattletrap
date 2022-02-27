module Rattletrap.Type.Attribute.PickupInfo where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data PickupInfo = PickupInfo
  { unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: U32.U32
  , unknown4 :: I32.I32
  , unknown5 :: I32.I32
  , unknown6 :: Bool
  , unknown7 :: Bool
  }
  deriving (Eq, Show)

instance Argo.HasCodec PickupInfo where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ PickupInfo
      <$> Argo.required unknown1 "unknown1"
      <*> Argo.required unknown2 "unknown2"
      <*> Argo.required unknown3 "unknown3"
      <*> Argo.required unknown4 "unknown4"
      <*> Argo.required unknown5 "unknown5"
      <*> Argo.required unknown6 "unknown6"
      <*> Argo.required unknown7 "unknown7"

bitPut :: PickupInfo -> BitPut.BitPut
bitPut x =
  BitPut.bool (unknown1 x)
    <> BitPut.bool (unknown2 x)
    <> U32.bitPut (unknown3 x)
    <> I32.bitPut (unknown4 x)
    <> I32.bitPut (unknown5 x)
    <> BitPut.bool (unknown6 x)
    <> BitPut.bool (unknown7 x)

bitGet :: BitGet.BitGet PickupInfo
bitGet = BitGet.label "PickupInfo" $ do
  unknown1 <- BitGet.label "unknown1" BitGet.bool
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  unknown3 <- BitGet.label "unknown3" U32.bitGet
  unknown4 <- BitGet.label "unknown4" I32.bitGet
  unknown5 <- BitGet.label "unknown5" I32.bitGet
  unknown6 <- BitGet.label "unknown6" BitGet.bool
  unknown7 <- BitGet.label "unknown7" BitGet.bool
  pure PickupInfo
    { unknown1
    , unknown2
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    , unknown7
    }
