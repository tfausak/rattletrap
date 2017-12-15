{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Message
  ( Message(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data Message = Message
  { messageFrame :: Word32le
  -- ^ Which frame this message belongs to, starting from 0.
  , messageName :: Str
  -- ^ The primary player's name.
  , messageValue :: Str
  -- ^ The content of the message.
  } deriving (Eq, Ord, Show)

$(deriveJson ''Message)
