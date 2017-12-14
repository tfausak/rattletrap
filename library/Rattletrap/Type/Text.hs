module Rattletrap.Type.Text
  ( Text(..)
  , stringToText
  , textToString
  ) where

import qualified Data.Text as Text

newtype Text = Text
  { textValue :: Text.Text
  } deriving (Eq, Ord, Show)

stringToText :: String -> Text
stringToText string = Text (Text.pack string)

textToString :: Text -> String
textToString text = Text.unpack (textValue text)
