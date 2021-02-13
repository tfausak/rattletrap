{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.TeamPaint where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data TeamPaintAttribute = TeamPaintAttribute
  { teamPaintAttributeTeam :: Word8le.Word8le
  , teamPaintAttributePrimaryColor :: Word8le.Word8le
  , teamPaintAttributeAccentColor :: Word8le.Word8le
  , teamPaintAttributePrimaryFinish :: Word32le.Word32le
  , teamPaintAttributeAccentFinish :: Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''TeamPaintAttribute)

putTeamPaintAttribute :: TeamPaintAttribute -> BitPut ()
putTeamPaintAttribute teamPaintAttribute = do
  Word8le.bitPut (teamPaintAttributeTeam teamPaintAttribute)
  Word8le.bitPut (teamPaintAttributePrimaryColor teamPaintAttribute)
  Word8le.bitPut (teamPaintAttributeAccentColor teamPaintAttribute)
  Word32le.bitPut (teamPaintAttributePrimaryFinish teamPaintAttribute)
  Word32le.bitPut (teamPaintAttributeAccentFinish teamPaintAttribute)

decodeTeamPaintAttributeBits :: BitGet TeamPaintAttribute
decodeTeamPaintAttributeBits =
  TeamPaintAttribute
    <$> Word8le.bitGet
    <*> Word8le.bitGet
    <*> Word8le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
