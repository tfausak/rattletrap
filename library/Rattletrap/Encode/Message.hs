module Rattletrap.Encode.Message
  ( putMessage
  ) where

import Rattletrap.Type.Message
import Rattletrap.Encode.Word32le
import Rattletrap.Encode.Text

import qualified Data.Binary as Binary

putMessage :: Message -> Binary.Put
putMessage message = do
  putWord32 (messageFrame message)
  putText (messageName message)
  putText (messageValue message)
