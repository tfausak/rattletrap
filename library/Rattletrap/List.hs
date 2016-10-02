module Rattletrap.List where

import Rattletrap.Word32

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Ord, Show)

getList :: Binary.Get a -> Binary.Get (List a)
getList getElement = do
  size <- getWord32
  elements <- Monad.replicateM (fromIntegral (word32Value size)) getElement
  pure (List elements)

putList :: (a -> Binary.Put) -> List a -> Binary.Put
putList putElement (List elements) = do
  putWord32 (Word32 (fromIntegral (length elements)))
  mapM_ putElement elements
