module Rattletrap.Type.Dictionary where

import Rattletrap.Decode.Common
import Rattletrap.Encode.Common
import qualified Rattletrap.Type.Str as Str

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Array as Array
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text

data Dictionary a = Dictionary
  { elements :: Array.Array Int (Str.Str, a)
  , lastKey :: Str.Str
  } deriving (Eq, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (Dictionary a) where
  parseJSON = Aeson.withObject "Dictionary" $ \ o -> do
    let
      required :: Aeson.FromJSON a => String -> Aeson.Parser a
      required k = o Aeson..: Text.pack k
    keys <- required "keys"
    lastKey_ <- required "last_key"
    value <- required "value"
    let
      build
        :: MonadFail m
        => Map.Map Text.Text a
        -> Int
        -> [(Int, (Str.Str, a))]
        -> [Text.Text]
        -> m (Array.Array Int (Str.Str, a))
      build m i xs ks = case ks of
        [] -> pure $ Array.array (0, i - 1) xs
        k : t -> case Map.lookup k m of
          Nothing -> fail $ "missing required key " <> show k
          Just v -> build m (i + 1) ((i, (Str.fromText k, v)) : xs) t
    elements_ <- build value 0 [] keys
    pure Dictionary { elements = elements_, lastKey = lastKey_ }

instance Aeson.ToJSON a => Aeson.ToJSON (Dictionary a) where
  toJSON x =
    let
      pair :: (Aeson.ToJSON v, Aeson.KeyValue kv) => String -> v -> kv
      pair k v = Text.pack k Aeson..= v
    in Aeson.object
      [ pair "keys" . fmap fst . Array.elems $ elements x
      , pair "last_key" $ lastKey x
      , pair "value" . Map.fromList . fmap (Bifunctor.first Str.toText) . Array.elems $ elements x
      ]

lookup :: Str.Str -> Dictionary a -> Maybe a
lookup k = Prelude.lookup k . Array.elems . elements

bytePut :: (a -> BytePut) -> Dictionary a -> BytePut
bytePut f x = do
  Monad.forM_ (elements x) $ \ (k, v) -> do
    Str.bytePut k
    f v
  Str.bytePut $ lastKey x

byteGet :: ByteGet a -> ByteGet (Dictionary a)
byteGet = byteGetWith 0 []

byteGetWith :: Int -> [(Int, (Str.Str, a))] -> ByteGet a -> ByteGet (Dictionary a)
byteGetWith i xs f = do
  k <- Str.byteGet
  if isNone k
    then pure Dictionary { elements = Array.array (0, i - 1) xs, lastKey = k }
    else do
      v <- f
      byteGetWith (i + 1) ((i, (k, v)) : xs) f

isNone :: Str.Str -> Bool
isNone = (== Text.pack "None") . Text.filter (/= '\x00') . Str.toText
