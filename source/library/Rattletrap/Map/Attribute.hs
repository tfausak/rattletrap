module Rattletrap.Map.Attribute
  ( AttributeMap(..)
  , getAttributeIdLimit
  , attributeMapLookup
  ) where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Word as Word

newtype AttributeMap =
  AttributeMap (HashMap.HashMap Word.Word32 Word32)
  deriving (Eq, Show)

getAttributeIdLimit :: AttributeMap -> Maybe Word
getAttributeIdLimit (AttributeMap attributeMap) = do
  let unsortedKeys = HashMap.keys attributeMap
  let sortedKeys = List.sortOn Ord.Down unsortedKeys
  maxStreamId <- Maybe.listToMaybe sortedKeys
  let limit = fromIntegral maxStreamId
  pure limit

attributeMapLookup :: CompressedWord -> AttributeMap -> Maybe Word32
attributeMapLookup streamId (AttributeMap attributeMap) =
  HashMap.lookup (fromIntegral (compressedWordValue streamId)) attributeMap
