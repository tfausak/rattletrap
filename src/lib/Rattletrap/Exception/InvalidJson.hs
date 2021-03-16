module Rattletrap.Exception.InvalidJson where

import qualified Control.Exception as Exception

newtype InvalidJson
  = InvalidJson String
  deriving (Eq, Show)

instance Exception.Exception InvalidJson
