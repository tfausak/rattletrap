module Rattletrap.Type.Word64
  ( Word64(..)
  ) where

import qualified Data.Word as Word

newtype Word64 = Word64
  { word64Value :: Word.Word64
  } deriving (Eq, Ord, Show)
