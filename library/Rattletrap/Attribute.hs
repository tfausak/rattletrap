module Rattletrap.Attribute where

import Rattletrap.AttributeValue
import Rattletrap.ClassPropertyMap
import Rattletrap.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeValue :: AttributeValue
  } deriving (Eq, Ord, Show)

getAttributes :: ClassPropertyMap
              -> CompressedWord
              -> BinaryBit.BitGet [Attribute]
getAttributes classPropertyMap actorId = do
  hasAttribute <- BinaryBit.getBool
  if not hasAttribute
    then pure []
    else do
      attribute <- getAttribute classPropertyMap actorId
      attributes <- getAttributes classPropertyMap actorId
      pure (attribute : attributes)

putAttributes :: [Attribute] -> BinaryBit.BitPut ()
putAttributes attributes = do
  mapM_ putAttribute attributes
  BinaryBit.putBool False

getAttribute :: ClassPropertyMap -> CompressedWord -> BinaryBit.BitGet Attribute
getAttribute classPropertyMap actorId = do
  let limit = getAttributeIdLimit classPropertyMap actorId
  id_ <- getCompressedWord limit
  let name = getAttributeName classPropertyMap actorId id_
  value <- getAttributeValue name
  pure Attribute {attributeId = id_, attributeValue = value}

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
