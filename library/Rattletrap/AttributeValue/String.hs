module Rattletrap.AttributeValue.String where

import Rattletrap.Text

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype StringAttributeValue = StringAttributeValue
  { stringAttributeValueValue :: Text
  } deriving (Eq, Ord, Show)

getStringAttributeValue :: BinaryBit.BitGet StringAttributeValue
getStringAttributeValue = do
  value <- getTextBits
  pure (StringAttributeValue value)

putStringAttributeValue :: StringAttributeValue -> BinaryBit.BitPut ()
putStringAttributeValue stringAttributeValue =
  putTextBits (stringAttributeValueValue stringAttributeValue)
