module Rattletrap.Exception.UnknownName where

import qualified Control.Exception as Exception
import qualified Data.Word as Word

newtype UnknownName
  = UnknownName Word.Word32
  deriving (Eq, Show)

instance Exception.Exception UnknownName
