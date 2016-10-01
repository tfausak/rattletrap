module Rattletrap.Dictionary where

import Rattletrap.Int32
import Rattletrap.Text

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy.Char8 as ByteString

newtype Dictionary a = Dictionary
  { dictionaryValue :: [(Text, a)]
  } deriving (Eq, Ord, Show)

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
noneKey = Text {textSize = Int32 5, textValue = ByteString.pack "None\x00"}
