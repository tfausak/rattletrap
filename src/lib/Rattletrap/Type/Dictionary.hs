module Rattletrap.Type.Dictionary where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Text as Text

data Dictionary a
  = Element Str.Str a (Dictionary a)
  | End Str.Str
  deriving (Eq, Show)

instance Json.FromJSON a => Json.FromJSON (Dictionary a) where
  parseJSON = Json.withObject
    "Dictionary"
    (\o -> do
      keys <- get o "keys"
      lastKey <- get o "last_key"
      value <- get o "value"
      Monad.foldM
        (\d k -> case Map.lookup k value of
          Nothing -> fail (unwords ["missing key", show k])
          Just v -> pure (Element (Str.fromText k) v d)
        )
        (End lastKey)
        (reverse keys)
    )

instance Json.ToJSON a => Json.ToJSON (Dictionary a) where
  toJSON d = Json.object
    [ pair "keys" (dictionaryKeys d)
    , pair "last_key" (dictionaryLastKey d)
    , pair "value" (dictionaryValue d)
    ]

dictionaryKeys :: Dictionary a -> [Str.Str]
dictionaryKeys = fmap fst . toList

dictionaryLastKey :: Dictionary a -> Str.Str
dictionaryLastKey x = case x of
  Element _ _ y -> dictionaryLastKey y
  End y -> y

lookup :: Str.Str -> Dictionary a -> Maybe a
lookup k x = case x of
  Element j v y -> if k == j then Just v else Rattletrap.Type.Dictionary.lookup k y
  End _ -> Nothing

dictionaryValue :: Dictionary a -> Map Text a
dictionaryValue = Map.mapKeys Str.toText . Map.fromList . toList

get :: Json.FromJSON a => Json.Object -> String -> Json.Parser a
get o k = o Json..: Text.pack k

pair :: Json.ToJSON a => String -> a -> (Text, Json.Value)
pair k v = (Text.pack k, Json.toJSON v)

toList :: Dictionary a -> [(Str.Str, a)]
toList x = case x of
  Element k v y -> (k, v) : toList y
  End _ -> []

bytePut :: (a -> BytePut) -> Dictionary a -> BytePut
bytePut f x = case x of
  Element k v y -> do
    Str.bytePut k
    f v
    bytePut f y
  End y -> Str.bytePut y

byteGet :: ByteGet a -> ByteGet (Dictionary a)
byteGet decodeValue = do
  key <- Str.byteGet
  case filter (/= '\x00') (Str.toString key) of
    "None" -> pure (End key)
    _ ->
      Element key <$> decodeValue <*> byteGet decodeValue
