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
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ TeamPaint
      <$> Argo.required team "team"
      <*> Argo.required primaryColor "primary_color"
      <*> Argo.required accentColor "accent_color"
      <*> Argo.required primaryFinish "primary_finish"
      <*> Argo.required accentFinish "accent_finish"

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
