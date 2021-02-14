module Rattletrap.Get where

import qualified Control.Applicative as Applicative

newtype Get s m a = Get (s -> m (Either String (s, a)))

instance Functor m => Functor (Get s m) where
  fmap f g = Get $ fmap (fmap (fmap f)) . run g

instance Monad m => Applicative (Get s m) where
  pure x = Get $ \ s -> pure $ Right (s, x)

  gf <*> gx = Get $ \ s1 -> do
    r <- run gf s1
    case r of
      Left e -> pure $ Left e
      Right (s2, f) -> run (fmap f gx) s2

instance Monad m => Monad (Get s m) where
  g >>= f = Get $ \ s1 -> do
    r <- run g s1
    case r of
      Left e -> pure $ Left e
      Right (s2, x) -> run (f x) s2

instance Monad m => MonadFail (Get s m) where
  fail = Get . const . pure . Left

instance Monad m => Applicative.Alternative (Get s m) where
  empty = fail "empty"

  gx <|> gy = Get $ \ s -> do
    r <- run gx s
    case r of
      Left _ -> run gy s
      Right x -> pure $ Right x

run :: Get s m a -> s -> m (Either String (s, a))
run (Get f) = f

get :: Applicative m => Get s m s
get = Get $ \ s -> pure $ Right (s, s)

put :: Applicative m => s -> Get s m ()
put s = Get $ \ _ -> pure $ Right (s, ())

lift :: Functor m => m a -> Get s m a
lift m = Get $ \ s -> fmap (\ x -> Right (s, x)) m
