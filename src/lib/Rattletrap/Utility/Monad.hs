module Rattletrap.Utility.Monad where

whenMaybe :: Applicative m => Bool -> m a -> m (Maybe a)
whenMaybe p f = if p then Just <$> f else pure Nothing
