module Rattletrap.Attribute where

import Rattletrap.AttributeValue
import Rattletrap.ClassAttributeMap
import Rattletrap.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeValue :: AttributeValue
  } deriving (Eq, Ord, Show)

getAttributes :: ClassAttributeMap
              -> CompressedWord
              -> BinaryBit.BitGet [Attribute]
getAttributes classAttributeMap actorId = do
  hasAttribute <- BinaryBit.getBool
  if not hasAttribute
    then pure []
    else do
      attribute <- getAttribute classAttributeMap actorId
      attributes <- getAttributes classAttributeMap actorId
      pure (attribute : attributes)

putAttributes :: [Attribute] -> BinaryBit.BitPut ()
putAttributes attributes = do
  mapM_ putAttribute attributes
  BinaryBit.putBool False

getAttribute :: ClassAttributeMap -> CompressedWord -> BinaryBit.BitGet Attribute
getAttribute classAttributeMap actorId = do
  let limit = getAttributeIdLimit classAttributeMap actorId
  id_ <- getCompressedWord limit
  let name = getAttributeName classAttributeMap actorId id_
  value <- getAttributeValue name
  pure Attribute {attributeId = id_, attributeValue = value}

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
