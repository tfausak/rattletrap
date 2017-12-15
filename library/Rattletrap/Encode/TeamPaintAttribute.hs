module Rattletrap.Encode.TeamPaintAttribute
  ( putTeamPaintAttribute
  ) where

import Rattletrap.Encode.Word32le
import Rattletrap.Encode.Word8le
import Rattletrap.Type.TeamPaintAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putTeamPaintAttribute :: TeamPaintAttribute -> BinaryBit.BitPut ()
putTeamPaintAttribute teamPaintAttribute = do
  putWord8Bits (teamPaintAttributeTeam teamPaintAttribute)
  putWord8Bits (teamPaintAttributePrimaryColor teamPaintAttribute)
  putWord8Bits (teamPaintAttributeAccentColor teamPaintAttribute)
  putWord32Bits (teamPaintAttributePrimaryFinish teamPaintAttribute)
  putWord32Bits (teamPaintAttributeAccentFinish teamPaintAttribute)
