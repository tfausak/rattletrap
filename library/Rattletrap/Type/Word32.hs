module Rattletrap.Type.Word32
  ( Word32(..)
  ) where

import qualified Data.Word as Word

newtype Word32 = Word32
  { word32Value :: Word.Word32
  } deriving (Eq, Ord, Show)
