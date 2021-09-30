{-# LANGUAGE FlexibleInstances #-}

module Rattletrap.Utility.Json where

import qualified Control.Applicative as Applicative
import qualified Data.Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Rattletrap.Json as Json
import qualified Rattletrap.Json.Array as Array
import qualified Rattletrap.Json.Boolean as Boolean
import qualified Rattletrap.Json.Null as Null
import qualified Rattletrap.Json.Number as Number
import qualified Rattletrap.Json.Object as Object
import qualified Rattletrap.Json.Pair as Pair
import qualified Rattletrap.Json.String as String
import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

type Value = Json.Value

data Parser a
  = Fail String
  | Pure a
  deriving (Eq, Show)

instance Functor Parser where
  fmap f p = case p of
    Fail e -> Fail e
    Pure x -> Pure $ f x

instance Applicative Parser where
  pure = Pure
  pf <*> px = case (pf, px) of
    (Pure f, Pure x) -> Pure $ f x
    (Fail e, _) -> Fail e
    (_, Fail e) -> Fail e

instance Monad Parser where
  p >>= f = case p of
    Fail e -> Fail e
    Pure x -> f x

instance MonadFail Parser where
  fail = Fail

instance Applicative.Alternative Parser where
  empty = fail "empty"
  px <|> py = case px of
    Pure x -> Pure x
    _ -> py

-- FromJSON -------------------------------------------------------------------

class FromJSON a where
  parseJSON :: Value -> Parser a

instance FromJSON () where
  parseJSON x = case x of
    Json.Array (Array.Array y) -> if Data.Array.elems y == []
      then pure ()
      else fail $ "parseJSON @() " <> show x
    _ -> fail $ "parseJSON @() " <> show x

instance FromJSON Bool where
  parseJSON x = case x of
    Json.Boolean (Boolean.Boolean y) -> pure y
    _ -> fail $ "parseJSON @Bool " <> show x

instance FromJSON Double where
  parseJSON x = case x of
    Json.Number y -> pure . realToFrac $ Number.toRational y
    _ -> fail $ "parseJSON @Double " <> show x

instance FromJSON Float where
  parseJSON x = case x of
    Json.Number y -> pure . realToFrac $ Number.toRational y
    _ -> fail $ "parseJSON @Float " <> show x

instance FromJSON Int where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Int " <> show x

instance FromJSON Int.Int8 where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Int8 " <> show x

instance FromJSON Int.Int32 where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Int32 " <> show x

instance FromJSON a => FromJSON (Map.Map Text.Text a) where
  parseJSON x = case x of
    Json.Object (Object.Object y) -> fmap Map.fromList
      . traverse (\ (Pair.Pair (String.String k) v) -> (,) k <$> parseJSON v)
      $ Data.Array.elems y
    _ -> fail $ "parseJSON @Map " <> show x

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON x = case x of
    Json.Null _ -> pure Nothing
    _ -> Just <$> parseJSON x

instance {-# OVERLAPPING #-} FromJSON String where
  parseJSON = fmap Text.unpack . parseJSON

instance FromJSON Text.Text where
  parseJSON x = case x of
    Json.String (String.String y) -> pure y
    _ -> fail $ "parseJSON @Text " <> show x

instance FromJSON Json.Value where
  parseJSON = pure

instance FromJSON Word where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Word " <> show x

instance FromJSON Word.Word8 where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Word8 " <> show x

instance FromJSON Word.Word16 where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Word16 " <> show x

instance FromJSON Word.Word32 where
  parseJSON x = case x of
    Json.Number y -> maybe (fail $ "parseJSON @Int " <> show x) (pure . fromInteger) $ Number.toInteger y
    _ -> fail $ "parseJSON @Word32 " <> show x

instance FromJSON a => FromJSON [a] where
  parseJSON x = case x of
    Json.Array (Array.Array y) -> traverse parseJSON $ Data.Array.elems y
    _ -> fail $ "parseJSON @[] " <> show x

instance (FromJSON a, FromJSON b) => FromJSON (a, b) where
  parseJSON x = do
    [y, z] <- parseJSON x
    (,) <$> parseJSON y <*> parseJSON z

-- ToJSON ---------------------------------------------------------------------

class ToJSON a where
  toJSON :: a -> Value

instance ToJSON () where
  toJSON = const . Json.Array . Array.Array $ listToArray []

instance ToJSON Bool where
  toJSON = Json.Boolean . Boolean.Boolean

instance ToJSON Char where
  toJSON = toJSON . Text.singleton

instance ToJSON Double where
  toJSON = Json.Number . Number.fromRational . toRational

instance ToJSON Float where
  toJSON = Json.Number . Number.fromRational . toRational

instance ToJSON Int where
  toJSON = Json.Number . integralToNumber

instance ToJSON Int.Int8 where
  toJSON = Json.Number . integralToNumber

instance ToJSON Int.Int32 where
  toJSON = Json.Number . integralToNumber

instance ToJSON a => ToJSON (Map.Map Text.Text a) where
  toJSON = Json.Object . Object.Object . listToArray . fmap (uncurry (.=)) . Map.toAscList

instance ToJSON a => ToJSON (Maybe a) where
  toJSON = maybe (Json.Null Null.Null) toJSON

instance {-# OVERLAPPING #-} ToJSON String where
  toJSON = toJSON . Text.pack

instance ToJSON String.String where
  toJSON = Json.String

instance ToJSON Text.Text where
  toJSON = toJSON . String.String

instance ToJSON Json.Value where
  toJSON = id

instance ToJSON Word where
  toJSON = Json.Number . integralToNumber

instance ToJSON Word.Word8 where
  toJSON = Json.Number . integralToNumber

instance ToJSON Word.Word16 where
  toJSON = Json.Number . integralToNumber

instance ToJSON Word.Word32 where
  toJSON = Json.Number . integralToNumber

instance ToJSON a => ToJSON [a] where
  toJSON = Json.Array . Array.Array . fmap toJSON . listToArray

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
  toJSON (x, y) = toJSON [toJSON x, toJSON y]

-------------------------------------------------------------------------------

integralToNumber :: Integral a => a -> Number.Number
integralToNumber = Number.normalize . flip Number.Number 0 . fromIntegral

encode :: ToJSON a => a -> LazyByteString.ByteString
encode = TextPut.toLazyByteString . Json.put . toJSON

encodePretty :: ToJSON a => a -> LazyByteString.ByteString
encodePretty = encode

decode :: FromJSON a => ByteString.ByteString -> Either String a
decode b = case Text.decodeUtf8' b of
  Left e -> Left $ show e
  Right t -> case TextGet.run Json.get t of
    Left e -> Left $ show e
    Right x -> case parseJSON x of
      Fail e -> Left e
      Pure y -> pure y

pair :: (KeyValue kv, ToJSON v) => String -> v -> kv
pair k v = Text.pack k .= v

class KeyValue kv where
  (.=) :: ToJSON v => Text.Text -> v -> kv

instance KeyValue (Pair.Pair String.String Value) where
  k .= v = Pair.Pair (String.String k) (toJSON v)

instance KeyValue (Text.Text, Value) where
  k .= v = (k, toJSON v)

object :: [Pair.Pair String.String Value] -> Value
object = Json.Object . Object.Object . listToArray

listToArray :: [a] -> Data.Array.Array Word a
listToArray xs = if null xs
  then Data.Array.array (1, 0) []
  else Data.Array.array (0, fromIntegral $ length xs - 1) (zip [0 ..] xs)

required :: FromJSON a => Object.Object (Pair.Pair String.String Value) -> String -> Parser a
required o k = do
  m <- optional o k
  maybe (Fail "") Pure m

optional :: FromJSON a => Object.Object (Pair.Pair String.String Value) -> String -> Parser (Maybe a)
optional (Object.Object o) k =
  let
    key = String.String $ Text.pack k
    tuples = fmap (\ (Pair.Pair x y) -> (x, y)) $ Data.Array.elems o
  in case lookup key tuples of
    Nothing -> pure Nothing
    Just (Json.Null _) -> pure Nothing
    Just value -> Just <$> parseJSON value

withText :: String -> (Text.Text -> Parser a) -> Value -> Parser a
withText _ f x = case x of
  Json.String (String.String y) -> f y
  _ -> fail "withText"

withObject :: String -> (Object.Object (Pair.Pair String.String Value) -> Parser a) -> Value -> Parser a
withObject _ f x = case x of
  Json.Object y -> f y
  _ -> fail "withObject"
