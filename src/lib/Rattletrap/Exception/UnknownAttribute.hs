module Rattletrap.Exception.UnknownAttribute where

import qualified Control.Exception as Exception

newtype UnknownAttribute
  = UnknownAttribute String
  deriving (Eq, Show)

instance Exception.Exception UnknownAttribute
