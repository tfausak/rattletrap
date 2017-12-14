{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Text
  ( Text(..)
  , stringToText
  , textToString
  ) where

import Rattletrap.Type.Common

import qualified Data.Text as Text

newtype Text = Text
  { textValue :: Text.Text
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Text where
  parseJSON = defaultParseJson "Text"

instance ToJSON Text where
  toEncoding = defaultToEncoding "Text"
  toJSON = defaultToJson "Text"

stringToText :: String -> Text
stringToText string = Text (Text.pack string)

textToString :: Text -> String
textToString text = Text.unpack (textValue text)
