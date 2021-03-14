module Rattletrap.Exception.MissingClassName where

import qualified Control.Exception as Exception

newtype MissingClassName
  = MissingClassName String
  deriving (Eq, Show)

instance Exception.Exception MissingClassName
