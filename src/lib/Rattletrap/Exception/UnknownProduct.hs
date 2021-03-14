module Rattletrap.Exception.UnknownProduct where

import qualified Control.Exception as Exception

newtype UnknownProduct
  = UnknownProduct String
  deriving (Eq, Show)

instance Exception.Exception UnknownProduct
