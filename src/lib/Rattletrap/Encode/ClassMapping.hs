module Rattletrap.Encode.ClassMapping
  ( putClassMapping
  ) where

import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.ClassMapping

import qualified Data.Binary as Binary

putClassMapping :: ClassMapping -> Binary.Put
putClassMapping classMapping = do
  putText (classMappingName classMapping)
  putWord32 (classMappingStreamId classMapping)
