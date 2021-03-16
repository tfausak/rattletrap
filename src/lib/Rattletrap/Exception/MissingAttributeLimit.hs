module Rattletrap.Exception.MissingAttributeLimit where

import qualified Control.Exception as Exception

newtype MissingAttributeLimit
  = MissingAttributeLimit Word
  deriving (Eq, Show)

instance Exception.Exception MissingAttributeLimit
