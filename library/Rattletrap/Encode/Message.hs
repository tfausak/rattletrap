module Rattletrap.Encode.Message
  ( putMessage
  ) where

import Rattletrap.Encode.Str
import Rattletrap.Encode.Word32le
import Rattletrap.Type.Message

import qualified Data.Binary as Binary

putMessage :: Message -> Binary.Put
putMessage message = do
  putWord32 (messageFrame message)
  putText (messageName message)
  putText (messageValue message)
