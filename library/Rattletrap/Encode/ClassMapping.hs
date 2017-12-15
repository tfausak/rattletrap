module Rattletrap.Encode.ClassMapping
  ( putClassMapping
  ) where

import Rattletrap.Type.ClassMapping
import Rattletrap.Encode.Word32le
import Rattletrap.Encode.Str

import qualified Data.Binary as Binary

putClassMapping :: ClassMapping -> Binary.Put
putClassMapping classMapping = do
  putText (classMappingName classMapping)
  putWord32 (classMappingStreamId classMapping)
