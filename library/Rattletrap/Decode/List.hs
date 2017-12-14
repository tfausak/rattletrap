module Rattletrap.Decode.List
  ( getList
  ) where

import Rattletrap.Primitive.Word32
import Rattletrap.Type.List

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary

getList :: Binary.Get a -> Binary.Get (List a)
getList getElement = do
  size <- getWord32
  elements <- Monad.replicateM (fromIntegral (word32Value size)) getElement
  pure (List elements)
