module Rattletrap.Get where

import qualified Control.Applicative as Applicative
import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Rattletrap.Exception.Empty as Empty
import qualified Rattletrap.Exception.Fail as Fail

newtype Get s m a = Get (s -> m (Either ([String], Exception.SomeException) (s, a)))

instance (Functor m) => Functor (Get s m) where
  fmap f g = Get $ fmap (fmap (fmap f)) . run g

instance (Monad m) => Applicative (Get s m) where
  pure x = Get $ \s -> pure $ Right (s, x)

  gf <*> gx = Get $ \s1 -> do
    r <- run gf s1
    case r of
      Left e -> pure $ Left e
      Right (s2, f) -> run (fmap f gx) s2

instance (Monad m) => Monad (Get s m) where
  g >>= f = Get $ \s1 -> do
    r <- run g s1
    case r of
      Left e -> pure $ Left e
      Right (s2, x) -> run (f x) s2

instance (Monad m) => MonadFail (Get s m) where
  fail = throw . Fail.Fail

instance (Monad m) => Applicative.Alternative (Get s m) where
  empty = throw Empty.Empty

  gx <|> gy = Get $ \s -> do
    r <- run gx s
    case r of
      Left _ -> run gy s
      Right x -> pure $ Right x

run :: Get s m a -> s -> m (Either ([String], Exception.SomeException) (s, a))
run (Get f) = f

get :: (Applicative m) => Get s m s
get = Get $ \s -> pure $ Right (s, s)

put :: (Applicative m) => s -> Get s m ()
put s = Get $ \_ -> pure $ Right (s, ())

lift :: (Functor m) => m a -> Get s m a
lift m = Get $ \s -> fmap (\x -> Right (s, x)) m

throw :: (Exception.Exception e, Applicative m) => e -> Get s m a
throw = Get . const . pure . Left . (,) [] . Exception.toException

embed :: (Monad m) => Get s m a -> s -> Get t m a
embed g s = do
  r <- lift $ run g s
  case r of
    Left (ls, e) -> labels ls $ throw e
    Right (_, x) -> pure x

labels :: (Functor m) => [String] -> Get s m a -> Get s m a
labels ls g = Get $ fmap (Bifunctor.first $ Bifunctor.first (ls <>)) . run g

label :: (Functor m) => String -> Get s m a -> Get s m a
label = labels . pure
