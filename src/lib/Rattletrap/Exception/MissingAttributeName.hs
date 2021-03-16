module Rattletrap.Exception.MissingAttributeName where

import qualified Control.Exception as Exception

newtype MissingAttributeName
  = MissingAttributeName Word
  deriving (Eq, Show)

instance Exception.Exception MissingAttributeName
