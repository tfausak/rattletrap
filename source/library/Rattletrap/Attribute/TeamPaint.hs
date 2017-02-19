module Rattletrap.Attribute.TeamPaint where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data TeamPaintAttribute = TeamPaintAttribute
  { teamPaintAttributeTeam :: Word8
  , teamPaintAttributePrimaryColor :: Word8
  , teamPaintAttributeAccentColor :: Word8
  , teamPaintAttributePrimaryFinish :: Word32
  , teamPaintAttributeAccentFinish :: Word32
  } deriving (Eq, Show)

getTeamPaintAttribute :: BinaryBit.BitGet TeamPaintAttribute
getTeamPaintAttribute = do
  team <- getWord8Bits
  primaryColor <- getWord8Bits
  accentColor <- getWord8Bits
  primaryFinish <- getWord32Bits
  accentFinish <- getWord32Bits
  pure
    (TeamPaintAttribute
       team
       primaryColor
       accentColor
       primaryFinish
       accentFinish)

putTeamPaintAttribute :: TeamPaintAttribute -> BinaryBit.BitPut ()
putTeamPaintAttribute teamPaintAttribute = do
  putWord8Bits (teamPaintAttributeTeam teamPaintAttribute)
  putWord8Bits (teamPaintAttributePrimaryColor teamPaintAttribute)
  putWord8Bits (teamPaintAttributeAccentColor teamPaintAttribute)
  putWord32Bits (teamPaintAttributePrimaryFinish teamPaintAttribute)
  putWord32Bits (teamPaintAttributeAccentFinish teamPaintAttribute)
