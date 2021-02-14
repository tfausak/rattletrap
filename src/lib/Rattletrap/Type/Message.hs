{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Message where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Message = Message
  { frame :: U32.U32
  -- ^ Which frame this message belongs to, starting from 0.
  , name :: Str.Str
  -- ^ The primary player's name.
  , value :: Str.Str
  -- ^ The content of the message.
  }
  deriving (Eq, Show)

$(deriveJson ''Message)

bytePut :: Message -> BytePut
bytePut message = do
  U32.bytePut (frame message)
  Str.bytePut (name message)
  Str.bytePut (value message)

byteGet :: ByteGet Message
byteGet = Message <$> U32.byteGet <*> Str.byteGet <*> Str.byteGet
