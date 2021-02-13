{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Message where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Message = Message
  { messageFrame :: Word32le.Word32le
  -- ^ Which frame this message belongs to, starting from 0.
  , messageName :: Str.Str
  -- ^ The primary player's name.
  , messageValue :: Str.Str
  -- ^ The content of the message.
  }
  deriving (Eq, Show)

$(deriveJson ''Message)

putMessage :: Message -> BytePut
putMessage message = do
  Word32le.bytePut (messageFrame message)
  Str.bytePut (messageName message)
  Str.bytePut (messageValue message)

decodeMessage :: ByteGet Message
decodeMessage = Message <$> Word32le.byteGet <*> Str.byteGet <*> Str.byteGet
