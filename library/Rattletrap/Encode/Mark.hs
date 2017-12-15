module Rattletrap.Encode.Mark
  ( putMark
  ) where

import Rattletrap.Type.Mark
import Rattletrap.Encode.Str
import Rattletrap.Encode.Word32le

import qualified Data.Binary as Binary

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)
