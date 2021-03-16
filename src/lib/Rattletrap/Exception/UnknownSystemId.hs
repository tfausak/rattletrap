module Rattletrap.Exception.UnknownSystemId where

import qualified Control.Exception as Exception
import qualified Data.Word as Word

newtype UnknownSystemId
  = UnknownSystemId Word.Word8
  deriving (Eq, Show)

instance Exception.Exception UnknownSystemId
