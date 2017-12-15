{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Dictionary
  ( Dictionary(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Str

data Dictionary a = Dictionary
  { dictionaryKeys :: [Str]
  -- ^ Objects in JSON aren't ordered, so the order of the keys must be stored
  -- separately.
  , dictionaryLastKey :: Str
  -- ^ The last key is usually @None@ but sometimes contains extra null bytes.
  , dictionaryValue :: Map Text a
  -- ^ Be sure to update 'dictionaryKeys' if you add, change, or remove a key
  -- in this map.
  } deriving (Eq, Ord, Show)

$(deriveJson ''Dictionary)
