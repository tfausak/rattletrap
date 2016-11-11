module Rattletrap.Mark where

import Rattletrap.Primitive.Text
import Rattletrap.Primitive.Word32

import qualified Data.Binary as Binary

data Mark = Mark
  { markValue :: Text
  , markFrame :: Word32
  } deriving (Eq, Ord, Show)

getMark :: Binary.Get Mark
getMark = do
  value <- getText
  frame <- getWord32
  pure (Mark value frame)

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)
