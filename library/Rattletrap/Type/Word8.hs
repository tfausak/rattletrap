module Rattletrap.Type.Word8
  ( Word8(..)
  ) where

import qualified Data.Word as Word

newtype Word8 = Word8
  { word8Value :: Word.Word8
  } deriving (Eq, Ord, Show)
