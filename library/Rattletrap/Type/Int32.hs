module Rattletrap.Type.Int32
  ( Int32(..)
  ) where

import qualified Data.Int as Int

newtype Int32 = Int32
  { int32Value :: Int.Int32
  } deriving (Eq, Ord, Show)
