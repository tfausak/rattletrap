module Rattletrap.Utility.Json
  ( module Rattletrap.Utility.Json
  , Aeson.FromJSON(parseJSON)
  , Aeson.ToJSON(toJSON)
  , Aeson.Value
  , Aeson.object
  , Aeson.withObject
  , Aeson.withText
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

required
  :: Aeson.FromJSON value => Aeson.Object -> String -> Aeson.Parser value
required object key = object Aeson..: Text.pack key

optional
  :: Aeson.FromJSON value
  => Aeson.Object
  -> String
  -> Aeson.Parser (Maybe value)
optional object key = object Aeson..:? Text.pack key

pair :: (Aeson.ToJSON value, Aeson.KeyValue pair) => String -> value -> pair
pair key value = Text.pack key Aeson..= value
