module Rattletrap.Decode.PrivateMatchSettingsAttribute
  ( decodePrivateMatchSettingsAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.PrivateMatchSettingsAttribute

decodePrivateMatchSettingsAttributeBits
  :: DecodeBits PrivateMatchSettingsAttribute
decodePrivateMatchSettingsAttributeBits =
  PrivateMatchSettingsAttribute
    <$> decodeStrBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeStrBits
    <*> decodeStrBits
    <*> getBool
