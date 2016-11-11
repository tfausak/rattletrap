module Rattletrap.Primitive.Dictionary where

import Rattletrap.Primitive.Text

import qualified Data.Binary as Binary

newtype Dictionary a = Dictionary
  { dictionaryValue :: [(Text, Maybe a)]
  } deriving (Eq, Ord, Show)

getDictionary :: Binary.Get a -> Binary.Get (Dictionary a)
getDictionary getValue = do
  key <- getText
  if isNoneKey key
    then pure (Dictionary [(key, Nothing)])
    else do
      value <- getValue
      let element = (key, Just value)
      Dictionary elements <- getDictionary getValue
      pure (Dictionary (element : elements))

putDictionary :: (a -> Binary.Put) -> Dictionary a -> Binary.Put
putDictionary putValue dictionary =
  mapM_
    (\(key, maybeValue) -> do
       putText key
       case maybeValue of
         Nothing -> pure ()
         Just value -> putValue value)
    (dictionaryValue dictionary)

isNoneKey :: Text -> Bool
isNoneKey text = filter (/= '\x00') (textToString text) == "None"
