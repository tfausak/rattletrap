module Rattletrap.Encode.Mark
  ( putMark
  ) where

import Rattletrap.Encode.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Mark

import qualified Data.Binary as Binary

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)
