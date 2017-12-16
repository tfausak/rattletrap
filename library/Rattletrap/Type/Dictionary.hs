{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Dictionary
  ( Dictionary(..)
  , dictionaryKeys
  , dictionaryLastKey
  , dictionaryValue
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Str

import qualified Data.Map as Map

data Dictionary a
  = DictionaryElement Str a (Dictionary a)
  | DictionaryEnd Str
  deriving (Eq, Ord, Show)

$(deriveJson ''Dictionary)

dictionaryKeys :: Dictionary a -> [Str]
dictionaryKeys = map fst . toList

dictionaryLastKey :: Dictionary a -> Str
dictionaryLastKey x = case x of
  DictionaryElement _ _ y -> dictionaryLastKey y
  DictionaryEnd y -> y

dictionaryValue :: Dictionary a -> Map Text a
dictionaryValue = Map.mapKeys strValue . Map.fromList . toList

toList :: Dictionary a -> [(Str, a)]
toList x = case x of
  DictionaryElement k v y -> (k, v) : toList y
  DictionaryEnd _ -> []
