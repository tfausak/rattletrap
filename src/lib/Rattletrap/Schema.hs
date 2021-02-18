module Rattletrap.Schema where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Rattletrap.Utility.Json as Json

data Schema = Schema
  { name :: Text.Text
  , json :: Aeson.Value
  } deriving (Eq, Show)

named :: String -> Aeson.Value -> Schema
named n j = Schema { name = Text.pack n, json = j }

ref :: Schema -> Aeson.Value
ref s = Aeson.object [ Json.pair "$ref" $ Text.pack "#/definitions/" <> name s ]

object :: [(Text.Text, Aeson.Value)] -> Aeson.Value
object xs = Aeson.object
  [ Json.pair "type" "object"
  , Json.pair "properties" $ Aeson.object xs
  , Json.pair "required" $ fmap fst xs
  ]

maybe :: Schema -> Schema
maybe s = Schema
  { name = Text.pack "maybe-" <> name s
  , json = Aeson.object [Json.pair "oneOf"
    [ ref s
    , Aeson.object [Json.pair "type" "null"]
    ]]
  }

array :: Schema -> Schema
array s = Schema
  { name = Text.pack "array-" <> name s
  , json = Aeson.object
    [ Json.pair "type" "array"
    , Json.pair "items" $ ref s
    ]
  }
