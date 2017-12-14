{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Message
  ( Message(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

data Message = Message
  { messageFrame :: Word32
  -- ^ Which frame this message belongs to, starting from 0.
  , messageName :: Text
  -- ^ The primary player's name.
  , messageValue :: Text
  -- ^ The content of the message.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Message where
  parseJSON = defaultParseJson "Message"

instance ToJSON Message where
  toEncoding = defaultToEncoding "Message"
  toJSON = defaultToJson "Message"