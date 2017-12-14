module Rattletrap.Decode.Mark
  ( getMark
  ) where

import Rattletrap.Type.Mark
import Rattletrap.Decode.Text
import Rattletrap.Decode.Word32

import qualified Data.Binary as Binary

getMark :: Binary.Get Mark
getMark = do
  value <- getText
  frame <- getWord32
  pure (Mark value frame)
