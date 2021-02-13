{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Message where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Message = Message
  { frame :: Word32le.Word32le
  -- ^ Which frame this message belongs to, starting from 0.
  , name :: Str.Str
  -- ^ The primary player's name.
  , value :: Str.Str
  -- ^ The content of the message.
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Message jsonOptions)

bytePut :: Message -> BytePut
bytePut message = do
  Word32le.bytePut (frame message)
  Str.bytePut (name message)
  Str.bytePut (value message)

byteGet :: ByteGet Message
byteGet = Message <$> Word32le.byteGet <*> Str.byteGet <*> Str.byteGet
