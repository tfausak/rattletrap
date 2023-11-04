{- hlint ignore "Avoid restricted extensions" -}
{-# LANGUAGE CPP #-}

module Rattletrap.Utility.Json
  ( module Rattletrap.Utility.Json,
    Aeson.FromJSON (parseJSON),
    Key.Key,
    Aeson.ToJSON (toJSON),
    Aeson.Value,
    Aeson.encode,
    Aeson.object,
    Aeson.withObject,
    Aeson.withText,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString

keyToString :: Key.Key -> String
keyToString = Key.toString

required ::
  (Aeson.FromJSON value) => Aeson.Object -> String -> Aeson.Parser value
required object key = object Aeson..: Key.fromString key

optional ::
  (Aeson.FromJSON value) =>
  Aeson.Object ->
  String ->
  Aeson.Parser (Maybe value)
optional object key = object Aeson..:? Key.fromString key

# if MIN_VERSION_aeson(2, 2, 0)
pair :: (Aeson.ToJSON value, Aeson.KeyValue e p) => String -> value -> p
# else
pair :: (Aeson.ToJSON value, Aeson.KeyValue p) => String -> value -> p
# endif
pair key value = Key.fromString key Aeson..= value

decode :: (Aeson.FromJSON a) => ByteString.ByteString -> Either String a
decode = Aeson.eitherDecodeStrict'

encodePretty :: (Aeson.ToJSON a) => a -> LazyByteString.ByteString
encodePretty =
  Aeson.encodePretty'
    Aeson.defConfig
      { Aeson.confCompare = compare,
        Aeson.confIndent = Aeson.Tab,
        Aeson.confTrailingNewline = True
      }
