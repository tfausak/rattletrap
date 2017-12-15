module Rattletrap.Decode.ClassMapping
  ( getClassMapping
  ) where

import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.ClassMapping

import qualified Data.Binary as Binary

getClassMapping :: Binary.Get ClassMapping
getClassMapping = do
  name <- getText
  streamId <- getWord32
  pure (ClassMapping name streamId)
