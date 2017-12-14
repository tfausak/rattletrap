module Rattletrap.Attribute.String where

import Rattletrap.Type.Text
import Rattletrap.Decode.Text
import Rattletrap.Encode.Text

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Text
  } deriving (Eq, Ord, Show)

getStringAttribute :: BinaryBit.BitGet StringAttribute
getStringAttribute = do
  value <- getTextBits
  pure (StringAttribute value)

putStringAttribute :: StringAttribute -> BinaryBit.BitPut ()
putStringAttribute stringAttribute =
  putTextBits (stringAttributeValue stringAttribute)
