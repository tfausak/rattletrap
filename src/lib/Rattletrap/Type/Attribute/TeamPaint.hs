{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.TeamPaint where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data TeamPaintAttribute = TeamPaintAttribute
  { teamPaintAttributeTeam :: Word8le
  , teamPaintAttributePrimaryColor :: Word8le
  , teamPaintAttributeAccentColor :: Word8le
  , teamPaintAttributePrimaryFinish :: Word32le
  , teamPaintAttributeAccentFinish :: Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''TeamPaintAttribute)

putTeamPaintAttribute :: TeamPaintAttribute -> BinaryBits.BitPut ()
putTeamPaintAttribute teamPaintAttribute = do
  putWord8Bits (teamPaintAttributeTeam teamPaintAttribute)
  putWord8Bits (teamPaintAttributePrimaryColor teamPaintAttribute)
  putWord8Bits (teamPaintAttributeAccentColor teamPaintAttribute)
  putWord32Bits (teamPaintAttributePrimaryFinish teamPaintAttribute)
  putWord32Bits (teamPaintAttributeAccentFinish teamPaintAttribute)

decodeTeamPaintAttributeBits :: DecodeBits TeamPaintAttribute
decodeTeamPaintAttributeBits =
  TeamPaintAttribute
    <$> decodeWord8leBits
    <*> decodeWord8leBits
    <*> decodeWord8leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
