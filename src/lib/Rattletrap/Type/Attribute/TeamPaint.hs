{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.TeamPaint where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data TeamPaint = TeamPaint
  { team :: U8.U8
  , primaryColor :: U8.U8
  , accentColor :: U8.U8
  , primaryFinish :: U32.U32
  , accentFinish :: U32.U32
  }
  deriving (Eq, Show)

$(deriveJson ''TeamPaint)

bitPut :: TeamPaint -> BitPut ()
bitPut teamPaintAttribute = do
  U8.bitPut (team teamPaintAttribute)
  U8.bitPut (primaryColor teamPaintAttribute)
  U8.bitPut (accentColor teamPaintAttribute)
  U32.bitPut (primaryFinish teamPaintAttribute)
  U32.bitPut (accentFinish teamPaintAttribute)

bitGet :: BitGet TeamPaint
bitGet =
  TeamPaint
    <$> U8.bitGet
    <*> U8.bitGet
    <*> U8.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
