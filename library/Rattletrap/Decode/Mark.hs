module Rattletrap.Decode.Mark
  ( getMark
  ) where

import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Mark

import qualified Data.Binary as Binary

getMark :: Binary.Get Mark
getMark = do
  value <- getText
  frame <- getWord32
  pure (Mark value frame)
