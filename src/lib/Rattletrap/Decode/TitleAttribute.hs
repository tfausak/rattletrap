module Rattletrap.Decode.TitleAttribute
  ( decodeTitleAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Type.TitleAttribute

decodeTitleAttributeBits :: DecodeBits TitleAttribute
decodeTitleAttributeBits =
  TitleAttribute
    <$> getBool
    <*> getBool
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> getBool
