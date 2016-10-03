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

getAttribute :: ClassAttributeMap
             -> CompressedWord
             -> BinaryBit.BitGet Attribute
getAttribute classAttributeMap actorId =
  case getAttributeIdLimit classAttributeMap actorId of
    Nothing -> fail ("could not get attribute ID limit for " ++ show actorId)
    Just limit -> do
      id_ <- getCompressedWord limit
      case getAttributeName classAttributeMap actorId id_ of
        Nothing -> fail ("could not get attribute name for " ++ show id_)
        Just name -> do
          value <- getAttributeValue name
          pure Attribute {attributeId = id_, attributeValue = value}

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
