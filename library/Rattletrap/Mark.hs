module Rattletrap.Mark where

import Rattletrap.Type.Text
import Rattletrap.Decode.Text
import Rattletrap.Encode.Text
import Rattletrap.Type.Word32
import Rattletrap.Decode.Word32
import Rattletrap.Encode.Word32

import qualified Data.Binary as Binary

data Mark = Mark
  { markValue :: Text
  -- ^ Which type of mark this is, like @Team0Goal@.
  , markFrame :: Word32
  -- ^ Which frame this mark belongs to, starting from 0.
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
