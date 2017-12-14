module Rattletrap.Encode.Mark
  ( putMark
  ) where

import Rattletrap.Type.Mark
import Rattletrap.Encode.Text
import Rattletrap.Encode.Word32

import qualified Data.Binary as Binary

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)