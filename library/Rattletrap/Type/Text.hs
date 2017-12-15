{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Text
  ( Text(..)
  , stringToText
  , textToString
  ) where

import Rattletrap.Type.Common

import qualified Data.Text as Text

newtype Text = Text
  { textValue :: Text.Text
  } deriving (Eq, Ord, Show)

$(deriveJson ''Text)

stringToText :: String -> Text
stringToText string = Text (Text.pack string)

textToString :: Text -> String
textToString text = Text.unpack (textValue text)
