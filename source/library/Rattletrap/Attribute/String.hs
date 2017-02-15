module Rattletrap.Attribute.String where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Text
  } deriving (Eq, Show)

getStringAttribute :: BinaryBit.BitGet StringAttribute
getStringAttribute = do
  value <- getTextBits
  pure (StringAttribute value)

putStringAttribute :: StringAttribute -> BinaryBit.BitPut ()
putStringAttribute stringAttribute =
  putTextBits (stringAttributeValue stringAttribute)
