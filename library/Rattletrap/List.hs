{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.List where

import Rattletrap.Word32

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data List a = List
  { listSize :: Word32
  , listValue :: [a]
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON a =>
         Aeson.FromJSON (List a)

instance Aeson.ToJSON a =>
         Aeson.ToJSON (List a)

getList :: Binary.Get a -> Binary.Get (List a)
getList getElement = do
  size <- getWord32
  elements <- Monad.replicateM (fromIntegral (word32Value size)) getElement
  pure List {listSize = size, listValue = elements}

putList :: (a -> Binary.Put) -> List a -> Binary.Put
putList putElement list = do
  putWord32 (listSize list)
  mapM_ putElement (listValue list)
