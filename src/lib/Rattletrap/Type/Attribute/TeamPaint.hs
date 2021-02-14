{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.TeamPaint where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data TeamPaintAttribute = TeamPaintAttribute
  { team :: Word8le.Word8le
  , primaryColor :: Word8le.Word8le
  , accentColor :: Word8le.Word8le
  , primaryFinish :: Word32le.Word32le
  , accentFinish :: Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''TeamPaintAttribute)

bitPut :: TeamPaintAttribute -> BitPut ()
bitPut teamPaintAttribute = do
  Word8le.bitPut (team teamPaintAttribute)
  Word8le.bitPut (primaryColor teamPaintAttribute)
  Word8le.bitPut (accentColor teamPaintAttribute)
  Word32le.bitPut (primaryFinish teamPaintAttribute)
  Word32le.bitPut (accentFinish teamPaintAttribute)

bitGet :: BitGet TeamPaintAttribute
bitGet =
  TeamPaintAttribute
    <$> Word8le.bitGet
    <*> Word8le.bitGet
    <*> Word8le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
