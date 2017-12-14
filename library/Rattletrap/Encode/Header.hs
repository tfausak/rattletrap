module Rattletrap.Encode.Header
  ( putHeader
  ) where

import Rattletrap.Type.Header
import Rattletrap.Encode.Word32
import Rattletrap.Encode.Text
import Rattletrap.Encode.Dictionary
import Rattletrap.Encode.Property

import qualified Data.Binary as Binary

putHeader :: Header -> Binary.Put
putHeader header = do
  putWord32 (headerEngineVersion header)
  putWord32 (headerLicenseeVersion header)
  case headerPatchVersion header of
    Nothing -> pure ()
    Just patchVersion -> putWord32 patchVersion
  putText (headerLabel header)
  putDictionary putProperty (headerProperties header)
