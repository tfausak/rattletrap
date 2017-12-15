{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Message
  ( Message(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Text

data Message = Message
  { messageFrame :: Word32le
  -- ^ Which frame this message belongs to, starting from 0.
  , messageName :: Text
  -- ^ The primary player's name.
  , messageValue :: Text
  -- ^ The content of the message.
  } deriving (Eq, Ord, Show)

$(deriveJson ''Message)
