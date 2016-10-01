{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Dictionary where

import Rattletrap.Text

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

newtype Dictionary a = Dictionary
  { dictionaryValue :: [(Text, a)]
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON a =>
         Aeson.FromJSON (Dictionary a)

instance Aeson.ToJSON a =>
         Aeson.ToJSON (Dictionary a)

getDictionary :: Binary.Get a -> Binary.Get (Dictionary a)
getDictionary getValue = do
  key <- getText
  if key == noneKey
    then pure (Dictionary [])
    else do
      value <- getValue
      let element = (key, value)
      Dictionary elements <- getDictionary getValue
      pure (Dictionary (element : elements))

putDictionary :: (a -> Binary.Put) -> Dictionary a -> Binary.Put
putDictionary putValue (Dictionary elements) = do
  mapM_
    (\(key, value) -> do
       putText key
       putValue value)
    elements
  putText noneKey

noneKey :: Text
noneKey = stringToText "None"
