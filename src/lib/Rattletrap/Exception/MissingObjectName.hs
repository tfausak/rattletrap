module Rattletrap.Exception.MissingObjectName where

import qualified Control.Exception as Exception
import qualified Data.Word as Word

newtype MissingObjectName
  = MissingObjectName Word.Word32
  deriving (Eq, Show)

instance Exception.Exception MissingObjectName
