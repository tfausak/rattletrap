module Rattletrap.Decode.Message
  ( decodeMessage
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Message

decodeMessage :: Decode Message
decodeMessage = Message <$> decodeWord32le <*> decodeStr <*> decodeStr
