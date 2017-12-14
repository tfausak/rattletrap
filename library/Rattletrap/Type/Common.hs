module Rattletrap.Type.Common
  ( deriveJson
  ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.TH as Json
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Language.Haskell.TH as TH

deriveJson :: TH.Name -> TH.Q [TH.Dec]
deriveJson name = Json.deriveJSON (jsonOptions $ TH.nameBase name) name

jsonOptions :: String -> Json.Options
jsonOptions prefix = Json.defaultOptions
  { Json.constructorTagModifier = toSnakeCase
  , Json.fieldLabelModifier = toSnakeCase . partialDropPrefix (lowerFirst prefix)
  , Json.omitNothingFields = True
  , Json.sumEncoding = Json.ObjectWithSingleField
  , Json.unwrapUnaryRecords = True
  }

lowerFirst :: String -> String
lowerFirst string = case string of
  "" -> string
  first : rest -> Char.toLower first : rest

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
