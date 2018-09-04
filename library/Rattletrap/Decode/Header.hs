module Rattletrap.Decode.Header
  ( decodeHeader
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Property
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Header
import Rattletrap.Type.Word32le

decodeHeader :: Decode Header
decodeHeader = do
  (major, minor) <- (,) <$> decodeWord32le <*> decodeWord32le
  Header major minor
    <$> decodeWhen
          (major >= Word32le 868 && minor >= Word32le 18)
          decodeWord32le
    <*> decodeStr
    <*> decodeDictionary decodeProperty
