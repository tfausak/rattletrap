{- hlint ignore "Avoid restricted extensions" -}
{-# LANGUAGE FlexibleInstances #-}

module Rattletrap.Utility.Json
  ( Argo.Value
  , Argo.FromValue(..)
  , Argo.ToValue(..)
  , decode
  , encode
  , encodePretty
  , object
  , optional
  , pair
  , required
  , Argo.withArray
  , Argo.withBoolean
  , Argo.withNumber
  , Argo.withObject
  , Argo.withString
  ) where

import qualified Argo
import qualified Argo.Class.FromValue as Argo
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Text as Text

type Parser = Argo.Result

decode :: Argo.FromValue a => ByteString.ByteString -> Either String a
decode x = case Argo.decode x of
  Argo.Failure e -> Left e
  Argo.Success y -> Right y

encode :: Argo.ToValue a => a -> LazyByteString.ByteString
encode = Builder.toLazyByteString . Argo.encode

encodePretty :: Argo.ToValue a => a -> LazyByteString.ByteString
encodePretty = encode

object :: [(Text.Text, Argo.Value)] -> Argo.Value
object = Argo.Object . fmap (\ (k, v) -> Argo.Member (Argo.Name k) v)

optional :: Argo.FromValue a => Argo.Object -> String -> Parser (Maybe a)
optional o ks =
  mapFailure (show ks <>)
    $ let kt = Text.pack ks
      in
        case List.find (\(Argo.Member (Argo.Name k) _) -> k == kt) o of
          Just (Argo.Member _ v) -> case v of
            Argo.Null -> pure Nothing
            _ -> Just <$> Argo.fromValue v
          Nothing -> pure Nothing

pair :: Argo.ToValue a => String -> a -> (Text.Text, Argo.Value)
pair k v = (Text.pack k, Argo.toValue v)

required :: Argo.FromValue a => Argo.Object -> String -> Parser a
required o ks = mapFailure (show ks <>) $ do
  let kt = Text.pack ks
  case List.find (\(Argo.Member (Argo.Name k) _) -> k == kt) o of
    Nothing -> Argo.Failure $ "missing required key " <> show kt
    Just (Argo.Member _ v) -> Argo.fromValue v

mapFailure :: (String -> String) -> Parser a -> Parser a
mapFailure f r = case r of
  Argo.Failure e -> Argo.Failure $ f e
  Argo.Success x -> Argo.Success x
