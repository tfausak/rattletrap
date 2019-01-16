module Rattletrap.Decode.StatTitle
  ( decodeStatTitleBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.FlaggedIntAttribute
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.StatTitle

decodeStatTitleBits :: DecodeBits StatTitle
decodeStatTitleBits = StatTitle
  <$> getBool
  <*> decodeStrBits
  <*> decodeFlaggedIntAttributeBits
  <*> decodeWord32leBits
