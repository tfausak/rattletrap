module Rattletrap.AttributeValue.TeamPaint where

import Rattletrap.Word32
import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data TeamPaintAttributeValue = TeamPaintAttributeValue
  { teamPaintAttributeValueTeam :: Word8
  , teamPaintAttributeValuePrimaryColor :: Word8
  , teamPaintAttributeValueAccentColor :: Word8
  , teamPaintAttributeValuePrimaryFinish :: Word32
  , teamPaintAttributeValueAccentFinish :: Word32
  } deriving (Eq, Ord, Show)

getTeamPaintAttributeValue :: BinaryBit.BitGet TeamPaintAttributeValue
getTeamPaintAttributeValue = do
  team <- getWord8Bits
  primaryColor <- getWord8Bits
  accentColor <- getWord8Bits
  primaryFinish <- getWord32Bits
  accentFinish <- getWord32Bits
  pure
    (TeamPaintAttributeValue
       team
       primaryColor
       accentColor
       primaryFinish
       accentFinish)

putTeamPaintAttributeValue :: TeamPaintAttributeValue -> BinaryBit.BitPut ()
putTeamPaintAttributeValue teamPaintAttributeValue = do
  putWord8Bits (teamPaintAttributeValueTeam teamPaintAttributeValue)
  putWord8Bits (teamPaintAttributeValuePrimaryColor teamPaintAttributeValue)
  putWord8Bits (teamPaintAttributeValueAccentColor teamPaintAttributeValue)
  putWord32Bits (teamPaintAttributeValuePrimaryFinish teamPaintAttributeValue)
  putWord32Bits (teamPaintAttributeValueAccentFinish teamPaintAttributeValue)
