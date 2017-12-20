module Rattletrap.Type.Dictionary
  ( Dictionary(..)
  , dictionaryKeys
  , dictionaryLastKey
  , dictionaryValue
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Str

import qualified Control.Monad as Monad
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Text as Text

data Dictionary a
  = DictionaryElement Str a (Dictionary a)
  | DictionaryEnd Str
  deriving (Eq, Ord, Show)

instance Json.FromJSON a => Json.FromJSON (Dictionary a) where
  parseJSON = Json.withObject "Dictionary" (\ o -> do
    keys <- get o "keys"
    lastKey <- get o "last_key"
    value <- get o "value"
    Monad.foldM
      (\ d k -> case Map.lookup k value of
        Nothing -> fail (unwords ["missing key", show k])
        Just v -> pure (DictionaryElement (Str k) v d))
      (DictionaryEnd lastKey)
      (reverse keys))

get :: Json.FromJSON a => Json.Object -> String -> Json.Parser a
get o k = o Json..: Text.pack k

instance Json.ToJSON a => Json.ToJSON (Dictionary a) where
  toJSON d = Json.object
    [ pair "keys" (dictionaryKeys d)
    , pair "last_key" (dictionaryLastKey d)
    , pair "value" (dictionaryValue d)
    ]

pair :: Json.ToJSON a => String -> a -> (Text, Json.Value)
pair k v = (Text.pack k, Json.toJSON v)

dictionaryKeys :: Dictionary a -> [Str]
dictionaryKeys = fmap fst . toList

dictionaryLastKey :: Dictionary a -> Str
dictionaryLastKey x = case x of
  DictionaryElement _ _ y -> dictionaryLastKey y
  DictionaryEnd y -> y

dictionaryValue :: Dictionary a -> Map Text a
dictionaryValue = Map.mapKeys strValue . Map.fromList . toList

toList :: Dictionary a -> [(Str, a)]
toList x = case x of
  DictionaryElement k v y -> (k, v) : toList y
  DictionaryEnd _ -> []
