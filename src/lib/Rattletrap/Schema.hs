module Rattletrap.Schema where

import qualified Data.Text as Text
import qualified Rattletrap.Utility.Json as Json

data Schema = Schema
  { name :: Text.Text
  , json :: Json.Value
  }
  deriving (Eq, Show)

named :: String -> Json.Value -> Schema
named n j = Schema { name = Text.pack n, json = j }

ref :: Schema -> Json.Value
ref s = Json.object [Json.pair "$ref" $ Text.pack "#/definitions/" <> name s]

object :: [((Text.Text, Json.Value), Bool)] -> Json.Value
object xs = Json.object
  [ Json.pair "type" "object"
  , Json.pair "properties" . Json.object $ fmap fst xs
  , Json.pair "required" . fmap (fst . fst) $ filter snd xs
  ]

maybe :: Schema -> Schema
maybe s = Schema
  { name = Text.pack "maybe-" <> name s
  , json = oneOf [ref s, json Rattletrap.Schema.null]
  }

oneOf :: [Json.Value] -> Json.Value
oneOf xs = Json.object [Json.pair "oneOf" xs]

tuple :: [Json.Value] -> Json.Value
tuple xs = Json.object
  [ Json.pair "type" "array"
  , Json.pair "items" xs
  , Json.pair "minItems" $ length xs
  , Json.pair "maxItems" $ length xs
  ]

array :: Schema -> Schema
array s = Schema
  { name = Text.pack "array-" <> name s
  , json = Json.object [Json.pair "type" "array", Json.pair "items" $ ref s]
  }

boolean :: Schema
boolean = named "boolean" $ Json.object [Json.pair "type" "boolean"]

integer :: Schema
integer = named "integer" $ Json.object [Json.pair "type" "integer"]

null :: Schema
null = named "null" $ Json.object [Json.pair "type" "null"]

number :: Schema
number = named "number" $ Json.object [Json.pair "type" "number"]

string :: Schema
string = named "string" $ Json.object [Json.pair "type" "string"]
