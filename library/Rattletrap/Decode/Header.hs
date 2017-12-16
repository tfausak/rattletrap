module Rattletrap.Decode.Header
  ( decodeHeader
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Property
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Header
import Rattletrap.Type.Word32le

decodeHeader :: Decode Header
decodeHeader = do
  (major, minor) <- (,) <$> getWord32 <*> getWord32
  Header major minor
    <$> decodePatch major minor
    <*> getText
    <*> decodeDictionary getProperty

decodePatch :: Word32le -> Word32le -> Decode (Maybe Word32le)
decodePatch major minor =
  if hasPatchVersion major minor then Just <$> getWord32 else pure Nothing

hasPatchVersion :: Word32le -> Word32le -> Bool
hasPatchVersion major minor = major >= Word32le 868 && minor >= Word32le 18
