module Rattletrap.Primitive.Dictionary where

import Rattletrap.Primitive.Text

import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Data.Text as Text

data Dictionary a = Dictionary
  { dictionaryKeys :: [Text]
  , dictionaryLastKey :: Text
  , dictionaryValue :: Map.Map Text.Text a
  } deriving (Eq, Ord, Show)

getDictionary :: Binary.Get a -> Binary.Get (Dictionary a)
getDictionary getValue = do
  (elements, lastKey) <- getElements getValue
  let keys = map fst elements
  let value = Map.mapKeys textValue (Map.fromList elements)
  pure (Dictionary keys lastKey value)

getElements :: Binary.Get a -> Binary.Get ([(Text, a)], Text)
getElements getValue = do
  (key, maybeValue) <- getElement getValue
  case maybeValue of
    Nothing -> pure ([], key)
    Just value -> do
      let element = (key, value)
      (elements, lastKey) <- getElements getValue
      pure (element : elements, lastKey)

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
       case Map.lookup (textValue key) elements of
         Nothing -> fail ("could not find key " ++ textToString key)
         Just value -> putValue value)
    (dictionaryKeys dictionary)
  putText (dictionaryLastKey dictionary)
