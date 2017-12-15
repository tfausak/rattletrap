module Rattletrap.Decode.List
  ( getList
  ) where

import Rattletrap.Decode.Word32le
import Rattletrap.Type.List
import Rattletrap.Type.Word32le

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary

getList :: Binary.Get a -> Binary.Get (List a)
getList getElement = do
  size <- getWord32
  elements <- Monad.replicateM (fromIntegral (word32leValue size)) getElement
  pure (List elements)
