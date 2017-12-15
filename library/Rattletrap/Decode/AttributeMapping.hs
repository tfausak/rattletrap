module Rattletrap.Decode.AttributeMapping
  ( getAttributeMapping
  ) where

import Rattletrap.Type.AttributeMapping
import Rattletrap.Decode.Word32le

import qualified Data.Binary as Binary

getAttributeMapping :: Binary.Get AttributeMapping
getAttributeMapping = do
  objectId <- getWord32
  streamId <- getWord32
  pure (AttributeMapping objectId streamId)
