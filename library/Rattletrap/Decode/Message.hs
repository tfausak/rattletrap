module Rattletrap.Decode.Message
  ( getMessage
  ) where

import Rattletrap.Type.Message
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Str

import qualified Data.Binary as Binary

getMessage :: Binary.Get Message
getMessage = do
  frame <- getWord32
  name <- getText
  value <- getText
  pure (Message frame name value)
