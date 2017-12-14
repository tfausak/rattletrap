module Rattletrap.Decode.ClassMapping
  ( getClassMapping
  ) where

import Rattletrap.Type.ClassMapping
import Rattletrap.Decode.Word32
import Rattletrap.Decode.Text

import qualified Data.Binary as Binary

getClassMapping :: Binary.Get ClassMapping
getClassMapping = do
  name <- getText
  streamId <- getWord32
  pure (ClassMapping name streamId)
