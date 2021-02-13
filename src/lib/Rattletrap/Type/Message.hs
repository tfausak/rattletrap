{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Message where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary

data Message = Message
  { messageFrame :: Word32le
  -- ^ Which frame this message belongs to, starting from 0.
  , messageName :: Str
  -- ^ The primary player's name.
  , messageValue :: Str
  -- ^ The content of the message.
  }
  deriving (Eq, Show)

$(deriveJson ''Message)

putMessage :: Message -> Binary.Put
putMessage message = do
  putWord32 (messageFrame message)
  putText (messageName message)
  putText (messageValue message)

decodeMessage :: Decode Message
decodeMessage = Message <$> decodeWord32le <*> decodeStr <*> decodeStr
