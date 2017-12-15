module Rattletrap.Decode.TeamPaintAttribute
  ( getTeamPaintAttribute
  ) where

import Rattletrap.Type.TeamPaintAttribute
import Rattletrap.Decode.Word8le
import Rattletrap.Decode.Word32

import qualified Data.Binary.Bits.Get as BinaryBit

getTeamPaintAttribute :: BinaryBit.BitGet TeamPaintAttribute
getTeamPaintAttribute = do
  team <- getWord8Bits
  primaryColor <- getWord8Bits
  accentColor <- getWord8Bits
  primaryFinish <- getWord32Bits
  accentFinish <- getWord32Bits
  pure
    ( TeamPaintAttribute
      team
      primaryColor
      accentColor
      primaryFinish
      accentFinish
    )
