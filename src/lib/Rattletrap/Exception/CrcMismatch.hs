module Rattletrap.Exception.CrcMismatch where

import qualified Control.Exception as Exception
import qualified Data.Word as Word

data CrcMismatch = CrcMismatch Word.Word32 Word.Word32
  deriving (Eq, Show)

instance Exception.Exception CrcMismatch where
  displayException (CrcMismatch x y) =
    unwords ["invalid CRC: expected", show x, "but got", show y]
