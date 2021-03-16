module Rattletrap.Exception.InvalidComponent where

import qualified Control.Exception as Exception

newtype InvalidComponent
  = InvalidComponent Word
  deriving (Eq, Show)

instance Exception.Exception InvalidComponent
