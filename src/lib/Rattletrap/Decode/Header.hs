module Rattletrap.Decode.Header
  ( decodeHeader
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Property
import Rattletrap.Decode.Str
import Rattletrap.Type.Header
import qualified Rattletrap.Type.Version as Version

decodeHeader :: Decode Header
decodeHeader = do
  Header
    <$> Version.fromBytes
    <*> decodeStr
    <*> decodeDictionary decodeProperty
