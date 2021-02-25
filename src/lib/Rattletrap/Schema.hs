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

object :: [((Text.Text, Aeson.Value), Bool)] -> Aeson.Value
object xs = Aeson.object
  [ Json.pair "type" "object"
  , Json.pair "properties" . Aeson.object $ fmap fst xs
  , Json.pair "required" . fmap (fst . fst) $ filter snd xs
  -- TODO: This is temporary to make sure I don't mess up any of the property
  -- names. Once the schema is done I should remove this because it's too
  -- restrictive.
  , Json.pair "additionalProperties" False
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

todo :: Schema
todo = named "todo" $ Aeson.toJSON True
