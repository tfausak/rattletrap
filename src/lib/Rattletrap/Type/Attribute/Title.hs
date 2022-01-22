module Rattletrap.Type.Attribute.Title where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data Title = Title
  { unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: U32.U32
  , unknown4 :: U32.U32
  , unknown5 :: U32.U32
  , unknown6 :: U32.U32
  , unknown7 :: U32.U32
  , unknown8 :: Bool
  }
  deriving (Eq, Show)

instance Argo.HasCodec Title where
  codec = Argo.fromObjectCodec Argo.Allow $ Title
    <$> Argo.project unknown1 (Argo.required (Argo.fromString "unknown1") Argo.codec)
    <*> Argo.project unknown2 (Argo.required (Argo.fromString "unknown2") Argo.codec)
    <*> Argo.project unknown3 (Argo.required (Argo.fromString "unknown3") Argo.codec)
    <*> Argo.project unknown4 (Argo.required (Argo.fromString "unknown4") Argo.codec)
    <*> Argo.project unknown5 (Argo.required (Argo.fromString "unknown5") Argo.codec)
    <*> Argo.project unknown6 (Argo.required (Argo.fromString "unknown6") Argo.codec)
    <*> Argo.project unknown7 (Argo.required (Argo.fromString "unknown7") Argo.codec)
    <*> Argo.project unknown8 (Argo.required (Argo.fromString "unknown8") Argo.codec)

bitPut :: Title -> BitPut.BitPut
bitPut titleAttribute =
  BitPut.bool (unknown1 titleAttribute)
    <> BitPut.bool (unknown2 titleAttribute)
    <> U32.bitPut (unknown3 titleAttribute)
    <> U32.bitPut (unknown4 titleAttribute)
    <> U32.bitPut (unknown5 titleAttribute)
    <> U32.bitPut (unknown6 titleAttribute)
    <> U32.bitPut (unknown7 titleAttribute)
    <> BitPut.bool (unknown8 titleAttribute)

bitGet :: BitGet.BitGet Title
bitGet = BitGet.label "Title" $ do
  unknown1 <- BitGet.label "unknown1" BitGet.bool
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  unknown3 <- BitGet.label "unknown3" U32.bitGet
  unknown4 <- BitGet.label "unknown4" U32.bitGet
  unknown5 <- BitGet.label "unknown5" U32.bitGet
  unknown6 <- BitGet.label "unknown6" U32.bitGet
  unknown7 <- BitGet.label "unknown7" U32.bitGet
  unknown8 <- BitGet.label "unknown8" BitGet.bool
  pure Title
    { unknown1
    , unknown2
    , unknown3
    , unknown4
    , unknown5
    , unknown6
    , unknown7
    , unknown8
    }
