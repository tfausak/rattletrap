module Rattletrap.Encode.List
  ( putList
  ) where

import Rattletrap.Encode.Word32le
import Rattletrap.Type.Word32le
import Rattletrap.Type.List

import qualified Data.Binary as Binary

putList :: (a -> Binary.Put) -> List a -> Binary.Put
putList putElement list = do
  let elements = listValue list
  putWord32 (Word32le (fromIntegral (length elements)))
  mapM_ putElement elements
