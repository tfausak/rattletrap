module Rattletrap.Encode.Header
  ( putHeader
  )
where

import Rattletrap.Encode.Dictionary
import Rattletrap.Encode.Property
import Rattletrap.Encode.Str
import Rattletrap.Type.Header
import qualified Rattletrap.Type.Version as Version

import qualified Data.Binary as Binary

putHeader :: Header -> Binary.Put
putHeader header = do
  Version.toBytes $ headerVersion header
  putText (headerLabel header)
  putDictionary putProperty (headerProperties header)
