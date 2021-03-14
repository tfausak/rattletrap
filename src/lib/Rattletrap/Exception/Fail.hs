module Rattletrap.Exception.Fail where

import qualified Control.Exception as Exception

newtype Fail
  = Fail String
  deriving (Eq, Show)

instance Exception.Exception Fail
