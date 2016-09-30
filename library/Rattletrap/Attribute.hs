module Rattletrap.Attribute where

import Rattletrap.AttributeValue
import Rattletrap.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeValue :: AttributeValue
  } deriving (Eq, Ord, Show)

getAttributes :: Word -> BinaryBit.BitGet [Attribute]
getAttributes limit = do
  hasAttribute <- BinaryBit.getBool
  if not hasAttribute
    then pure []
    else do
      attribute <- getAttribute limit
      attributes <- getAttributes limit
      pure (attribute : attributes)

putAttributes :: [Attribute] -> BinaryBit.BitPut ()
putAttributes attributes = do
  mapM_ putAttribute attributes
  BinaryBit.putBool False

getAttribute :: Word -> BinaryBit.BitGet Attribute
getAttribute limit = do
  id_ <- getCompressedWord limit
  value <- getAttributeValue
  pure Attribute {attributeId = id_, attributeValue = value}

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
