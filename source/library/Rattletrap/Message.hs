module Rattletrap.Message where

import Rattletrap.Primitive

import qualified Data.Binary as Binary

data Message = Message
  { messageFrame :: Word32
  -- ^ Which frame this message belongs to, starting from 0.
  , messageName :: Text
  -- ^ The primary player's name.
  , messageValue :: Text
  -- ^ The content of the message.
  } deriving (Eq, Show)

getMessage :: Binary.Get Message
getMessage = do
  frame <- getWord32
  name <- getText
  value <- getText
  pure (Message frame name value)

putMessage :: Message -> Binary.Put
putMessage message = do
  putWord32 (messageFrame message)
  putText (messageName message)
  putText (messageValue message)
