module Rattletrap.Utility.Json where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

pair :: (Aeson.ToJSON value, Aeson.KeyValue pair) => String -> value -> pair
pair key value = Text.pack key Aeson..= value
