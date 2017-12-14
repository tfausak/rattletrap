module Rattletrap.Decode.Message
  ( getMessage
  ) where

import Rattletrap.Type.Message
import Rattletrap.Decode.Word32
import Rattletrap.Decode.Text

import qualified Data.Binary as Binary

getMessage :: Binary.Get Message
getMessage = do
  frame <- getWord32
  name <- getText
  value <- getText
  pure (Message frame name value)
