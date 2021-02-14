module Rattletrap.Decode.Common
  ( decodeWhen
  ) where

import qualified Control.Applicative as Applicative

decodeWhen
  :: (Applicative m, Applicative.Alternative f) => Bool -> m a -> m (f a)
decodeWhen p f = if p then fmap pure f else pure Applicative.empty
