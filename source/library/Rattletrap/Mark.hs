module Rattletrap.Mark where

import Rattletrap.Primitive

import qualified Data.Binary as Binary

data Mark = Mark
  { markValue :: Text
  -- ^ Which type of mark this is, like @Team0Goal@.
  , markFrame :: Word32
  -- ^ Which frame this mark belongs to, starting from 0.
  } deriving (Eq, Show)

getMark :: Binary.Get Mark
getMark = do
  value <- getText
  frame <- getWord32
  pure (Mark value frame)

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)
