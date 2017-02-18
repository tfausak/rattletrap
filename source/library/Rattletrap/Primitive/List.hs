module Rattletrap.Primitive.List where

import Rattletrap.Primitive.Word32

import qualified Data.Binary as Binary
import qualified Data.Vector as Vector

newtype List a = List
  { listVector :: Vector.Vector a
  } deriving (Eq, Show)

getList :: Binary.Get a -> Binary.Get (List a)
getList getElement = do
  size <- getWord32
  elements <- Vector.replicateM (fromIntegral (word32Value size)) getElement
  pure (List elements)

putList :: (a -> Binary.Put) -> List a -> Binary.Put
putList putElement list = do
  let elements = listVector list
  putWord32 (Word32 (fromIntegral (length elements)))
  mapM_ putElement elements
