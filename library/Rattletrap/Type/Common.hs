{-# LANGUAGE FlexibleContexts #-}

module Rattletrap.Type.Common
  ( Ghc.Generic
  , Json.ToJSON
  , Json.toJSON
  , defaultToJson
  , Json.toEncoding
  , defaultToEncoding
  , Json.FromJSON
  , Json.parseJSON
  , defaultParseJson
  ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified GHC.Generics as Ghc

defaultParseJson
  :: (Ghc.Generic a, Json.GFromJSON Json.Zero (Ghc.Rep a))
  => String
  -> Json.Value
  -> Json.Parser a
defaultParseJson = Json.genericParseJSON . jsonOptions

defaultToEncoding
  :: (Ghc.Generic a, Json.GToEncoding Json.Zero (Ghc.Rep a))
  => String
  -> a
  -> Json.Encoding
defaultToEncoding = Json.genericToEncoding . jsonOptions

defaultToJson
  :: (Ghc.Generic a, Json.GToJSON Json.Zero (Ghc.Rep a))
  => String
  -> a
  -> Json.Value
defaultToJson = Json.genericToJSON . jsonOptions

jsonOptions :: String -> Json.Options
jsonOptions prefix = Json.defaultOptions
  { Json.constructorTagModifier = toSnakeCase . partialDropPrefix prefix
  , Json.fieldLabelModifier = toSnakeCase
    . partialDropPrefix (lowerFirst prefix)
  , Json.omitNothingFields = True
  , Json.sumEncoding = Json.ObjectWithSingleField
  , Json.unwrapUnaryRecords = True
  }

lowerFirst :: String -> String
lowerFirst string = case string of
  "" -> string
  first:rest -> Char.toLower first : rest

toSnakeCase :: String -> String
toSnakeCase = Json.camelTo2 '_'

partialDropPrefix :: (Eq a, Show a) => [a] -> [a] -> [a]
partialDropPrefix prefix list =
  Maybe.fromMaybe
      (error $ unwords [show prefix, "is not a prefix of", show list])
    $ dropPrefix prefix list

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix prefix list = case prefix of
  [] -> Just list
  ph:pt -> case list of
    [] -> Nothing
    lh:lt -> if ph == lh then dropPrefix pt lt else Nothing
