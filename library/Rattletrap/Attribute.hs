module Rattletrap.Attribute where

import Rattletrap.AttributeValue
import Rattletrap.CompressedWord
import Rattletrap.Text

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeValue :: AttributeValue
  } deriving (Eq, Ord, Show)

getAttributes :: Word
              -> (CompressedWord -> Text)
              -> BinaryBit.BitGet [Attribute]
getAttributes limit getName = do
  hasAttribute <- BinaryBit.getBool
  if not hasAttribute
    then pure []
    else do
      attribute <- getAttribute limit getName
      attributes <- getAttributes limit getName
      pure (attribute : attributes)

putAttributes :: [Attribute] -> BinaryBit.BitPut ()
putAttributes attributes = do
  mapM_ putAttribute attributes
  BinaryBit.putBool False

getAttribute :: Word -> (CompressedWord -> Text) -> BinaryBit.BitGet Attribute
getAttribute limit getName = do
  id_ <- getCompressedWord limit
  value <- getAttributeValue (getName id_)
  pure Attribute {attributeId = id_, attributeValue = value}

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
