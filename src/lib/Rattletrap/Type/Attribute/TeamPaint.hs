module Rattletrap.Type.Attribute.TeamPaint where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

data TeamPaint = TeamPaint
  { team :: U8.U8
  , primaryColor :: U8.U8
  , accentColor :: U8.U8
  , primaryFinish :: U32.U32
  , accentFinish :: U32.U32
  }
  deriving (Eq, Show)

$(deriveJson ''TeamPaint)

schema :: Schema.Schema
schema = Schema.named "attribute-team-paint" $ Schema.object
  [ (Json.pair "team" $ Schema.ref U8.schema, True)
  , (Json.pair "primary_color" $ Schema.ref U8.schema, True)
  , (Json.pair "accent_color" $ Schema.ref U8.schema, True)
  , (Json.pair "primary_finish" $ Schema.ref U32.schema, True)
  , (Json.pair "accent_finish" $ Schema.ref U32.schema, True)
  ]

bitPut :: TeamPaint -> BitPut.BitPut
bitPut teamPaintAttribute =
  U8.bitPut (team teamPaintAttribute)
    <> U8.bitPut (primaryColor teamPaintAttribute)
    <> U8.bitPut (accentColor teamPaintAttribute)
    <> U32.bitPut (primaryFinish teamPaintAttribute)
    <> U32.bitPut (accentFinish teamPaintAttribute)

bitGet :: BitGet.BitGet TeamPaint
bitGet =
  TeamPaint
    <$> U8.bitGet
    <*> U8.bitGet
    <*> U8.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
