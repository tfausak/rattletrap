module Rattletrap.Decode.Dictionary
  ( getDictionary
  ) where

import Rattletrap.Decode.Str
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Str

import qualified Data.Binary as Binary
import qualified Data.Map as Map

getDictionary :: Binary.Get a -> Binary.Get (Dictionary a)
getDictionary getValue = do
  (elements, lastKey) <- getElements getValue
  let keys = fmap fst elements
  let value = Map.mapKeys strValue (Map.fromList elements)
  pure (Dictionary keys lastKey value)

getElements :: Binary.Get a -> Binary.Get ([(Str, a)], Str)
getElements getValue = do
  (key, maybeValue) <- getElement getValue
  case maybeValue of
    Nothing -> pure ([], key)
    Just value -> do
      let element = (key, value)
      (elements, lastKey) <- getElements getValue
      pure (element : elements, lastKey)

getElement :: Binary.Get a -> Binary.Get (Str, Maybe a)
getElement getValue = do
  key <- getText
  if isNoneKey key
    then pure (key, Nothing)
    else do
      value <- getValue
      pure (key, Just value)

isNoneKey :: Str -> Bool
isNoneKey text = filter (/= '\x00') (fromStr text) == "None"
