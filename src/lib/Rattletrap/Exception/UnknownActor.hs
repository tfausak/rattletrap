module Rattletrap.Exception.UnknownActor where

import qualified Control.Exception as Exception

newtype UnknownActor
  = UnknownActor Word
  deriving (Eq, Show)

instance Exception.Exception UnknownActor
