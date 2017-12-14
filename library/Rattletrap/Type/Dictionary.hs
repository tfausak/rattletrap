module Rattletrap.Type.Dictionary
  ( Dictionary(..)
  ) where

import Rattletrap.Type.Text

import qualified Data.Map as Map
import qualified Data.Text as Text

data Dictionary a = Dictionary
  { dictionaryKeys :: [Text]
  -- ^ Objects in JSON aren't ordered, so the order of the keys must be stored
  -- separately.
  , dictionaryLastKey :: Text
  -- ^ The last key is usually @None@ but sometimes contains extra null bytes.
  , dictionaryValue :: Map.Map Text.Text a
  -- ^ Be sure to update 'dictionaryKeys' if you add, change, or remove a key
  -- in this map.
  } deriving (Eq, Ord, Show)
