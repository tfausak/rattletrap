module Rattletrap.Encode.Header
  ( putHeader
  )
where

import Rattletrap.Encode.Dictionary
import Rattletrap.Encode.Property
import Rattletrap.Encode.Str
import Rattletrap.Encode.Word32le
import Rattletrap.Type.Header

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
