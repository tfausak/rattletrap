module Rattletrap.Type.Dictionary where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

data Dictionary a = Dictionary
  { elements :: List.List (Str.Str, a)
  , lastKey :: Str.Str
  }
  deriving (Eq, Show)

instance Json.FromJSON a => Json.FromJSON (Dictionary a) where
  parseJSON = Json.withObject "Dictionary" $ \o -> do
    keys <- Json.required o "keys"
    lastKey_ <- Json.required o "last_key"
    value <- Json.required o "value"
    let
      build
        :: MonadFail m
        => Map.Map Text.Text a
        -> Int
        -> [(Int, (Str.Str, a))]
        -> [Text.Text]
        -> m (List.List (Str.Str, a))
      build m i xs ks = case ks of
        [] -> pure . List.fromList . reverse $ fmap snd xs
        k : t -> case Map.lookup k m of
          Nothing -> fail $ "missing required key " <> show k
          Just v -> build m (i + 1) ((i, (Str.fromText k, v)) : xs) t
    elements_ <- build value 0 [] keys
    pure Dictionary { elements = elements_, lastKey = lastKey_ }

instance Json.ToJSON a => Json.ToJSON (Dictionary a) where
  toJSON x = Json.object
    [ Json.pair "keys" . fmap fst . List.toList $ elements x
    , Json.pair "last_key" $ lastKey x
    , Json.pair "value"
    . Map.fromList
    . fmap (Bifunctor.first Str.toText)
    . List.toList
    $ elements x
    ]

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named ("dictionary-" <> Text.unpack (Schema.name s)) $ Schema.object
    [ (Json.pair "keys" . Schema.json $ Schema.array Str.schema, True)
    , (Json.pair "last_key" $ Schema.ref Str.schema, True)
    , ( Json.pair "value" $ Json.object
        [ Json.pair "type" "object"
        , Json.pair "additionalProperties" $ Schema.ref s
        ]
      , True
      )
    ]

lookup :: Str.Str -> Dictionary a -> Maybe a
lookup k = Prelude.lookup k . List.toList . elements

bytePut :: (a -> BytePut.BytePut) -> Dictionary a -> BytePut.BytePut
bytePut f x =
  foldMap (\(k, v) -> Str.bytePut k <> f v) (List.toList $ elements x)
    <> Str.bytePut (lastKey x)

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Dictionary a)
byteGet = ByteGet.label "Dictionary" . byteGetWith 0 []

byteGetWith
  :: Int
  -> [(Int, (Str.Str, a))]
  -> ByteGet.ByteGet a
  -> ByteGet.ByteGet (Dictionary a)
byteGetWith i xs f = do
  k <- ByteGet.label ("key (" <> show i <> ")") Str.byteGet
  if isNone k
    then pure Dictionary
      { elements = List.fromList . reverse $ fmap snd xs
      , lastKey = k
      }
    else do
      v <- ByteGet.label ("value (" <> Str.toString k <> ")") f
      byteGetWith (i + 1) ((i, (k, v)) : xs) f

isNone :: Str.Str -> Bool
isNone = (== Text.pack "None") . Text.filter (/= '\x00') . Str.toText
