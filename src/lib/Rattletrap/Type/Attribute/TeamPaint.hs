module Rattletrap.Type.Attribute.TeamPaint where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

data TeamPaint = TeamPaint
  { team :: U8.U8
  , primaryColor :: U8.U8
  , accentColor :: U8.U8
  , primaryFinish :: U32.U32
  , accentFinish :: U32.U32
  }
  deriving (Eq, Show)

instance Argo.HasCodec TeamPaint where
  codec = Argo.identified .
    Argo.fromObjectCodec Argo.Allow
      $ TeamPaint
      <$> Argo.project team (Argo.required (Argo.fromString "team") Argo.codec)
      <*> Argo.project
            primaryColor
            (Argo.required (Argo.fromString "primary_color") Argo.codec)
      <*> Argo.project
            accentColor
            (Argo.required (Argo.fromString "accent_color") Argo.codec)
      <*> Argo.project
            primaryFinish
            (Argo.required (Argo.fromString "primary_finish") Argo.codec)
      <*> Argo.project
            accentFinish
            (Argo.required (Argo.fromString "accent_finish") Argo.codec)

bitPut :: TeamPaint -> BitPut.BitPut
bitPut teamPaintAttribute =
  U8.bitPut (team teamPaintAttribute)
    <> U8.bitPut (primaryColor teamPaintAttribute)
    <> U8.bitPut (accentColor teamPaintAttribute)
    <> U32.bitPut (primaryFinish teamPaintAttribute)
    <> U32.bitPut (accentFinish teamPaintAttribute)

bitGet :: BitGet.BitGet TeamPaint
bitGet = BitGet.label "TeamPaint" $ do
  team <- BitGet.label "team" U8.bitGet
  primaryColor <- BitGet.label "primaryColor" U8.bitGet
  accentColor <- BitGet.label "accentColor" U8.bitGet
  primaryFinish <- BitGet.label "primaryFinish" U32.bitGet
  accentFinish <- BitGet.label "accentFinish" U32.bitGet
  pure TeamPaint
    { team
    , primaryColor
    , accentColor
    , primaryFinish
    , accentFinish
    }
