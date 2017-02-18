module Rattletrap.Primitive.Dictionary where

import Rattletrap.Primitive.Text

import qualified Data.Binary as Binary
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data Dictionary a = Dictionary
  { dictionaryKeys :: Vector.Vector Text
  -- ^ Objects in JSON aren't ordered, so the order of the keys must be stored
  -- separately.
  , dictionaryLastKey :: Text
  -- ^ The last key is usually @None@ but sometimes contains extra null bytes.
  , dictionaryValue :: HashMap.HashMap Text.Text a
  -- ^ Be sure to update 'dictionaryKeys' if you add, change, or remove a key
  -- in this map.
  } deriving (Eq, Show)

getDictionary :: Binary.Get a -> Binary.Get (Dictionary a)
getDictionary getValue = do
  (elements, lastKey) <- getElements getValue
  let keys = Vector.fromList (map fst elements)
  let value = HashMap.fromList (map (\(k, v) -> (textValue k, v)) elements)
  pure (Dictionary keys lastKey value)

getElements :: Binary.Get a -> Binary.Get ([(Text, a)], Text)
getElements getValue = do
  let go elements = do
        (key, maybeValue) <- getElement getValue
        case maybeValue of
          Nothing -> pure (reverse elements, key)
          Just value -> do
            let element = (key, value)
            go (element : elements)
  go []

getElement :: Binary.Get a -> Binary.Get (Text, Maybe a)
getElement getValue = do
  key <- getText
  if isNoneKey key
    then pure (key, Nothing)
    else do
      value <- getValue
      pure (key, Just value)

isNoneKey :: Text -> Bool
isNoneKey text = filter (/= '\x00') (textToString text) == "None"

putDictionary :: (a -> Binary.Put) -> Dictionary a -> Binary.Put
putDictionary putValue dictionary = do
  let elements = dictionaryValue dictionary
  mapM_
    (\key -> do
       putText key
       case HashMap.lookup (textValue key) elements of
         Nothing -> fail ("could not find key " ++ textToString key)
         Just value -> putValue value)
    (dictionaryKeys dictionary)
  putText (dictionaryLastKey dictionary)
