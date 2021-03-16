module Rattletrap.Exception.MissingProductName where

import qualified Control.Exception as Exception
import qualified Data.Word as Word

newtype MissingProductName
  = MissingProductName Word.Word32
  deriving (Eq, Show)

instance Exception.Exception MissingProductName
