module Rattletrap.Exception.UnknownProperty where

import qualified Control.Exception as Exception

newtype UnknownProperty
  = UnknownProperty String
  deriving (Eq, Show)

instance Exception.Exception UnknownProperty
