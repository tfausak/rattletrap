{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le

import qualified Data.Binary as Binary

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Ord, Show)

$(deriveJson ''List)

putList :: (a -> Binary.Put) -> List a -> Binary.Put
putList putElement list = do
  let elements = listValue list
  putWord32 (Word32le (fromIntegral (length elements)))
  mapM_ putElement elements
