module Rattletrap.Decode.Header
  ( getHeader
  ) where

import Rattletrap.Type.Header
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Str
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Property

import qualified Data.Binary as Binary

getHeader :: Binary.Get Header
getHeader = do
  engineVersion <- getWord32
  licenseeVersion <- getWord32
  patchVersion <- getPatchVersion engineVersion licenseeVersion
  label <- getText
  properties <- getDictionary getProperty
  pure (Header engineVersion licenseeVersion patchVersion label properties)

getPatchVersion :: Word32le -> Word32le -> Binary.Get (Maybe Word32le)
getPatchVersion major minor = if hasPatchVersion major minor
  then do
    patchVersion <- getWord32
    pure (Just patchVersion)
  else pure Nothing

hasPatchVersion :: Word32le -> Word32le -> Bool
hasPatchVersion major minor = major >= Word32le 868 && minor >= Word32le 18
