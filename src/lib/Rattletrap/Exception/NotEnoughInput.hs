module Rattletrap.Exception.NotEnoughInput where

import qualified Control.Exception as Exception

data NotEnoughInput
  = NotEnoughInput
  deriving (Eq, Show)

instance Exception.Exception NotEnoughInput
