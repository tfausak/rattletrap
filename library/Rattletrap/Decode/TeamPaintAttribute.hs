module Rattletrap.Decode.TeamPaintAttribute
  ( decodeTeamPaintAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.TeamPaintAttribute

decodeTeamPaintAttributeBits :: DecodeBits TeamPaintAttribute
decodeTeamPaintAttributeBits =
  TeamPaintAttribute
    <$> decodeWord8leBits
    <*> decodeWord8leBits
    <*> decodeWord8leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
