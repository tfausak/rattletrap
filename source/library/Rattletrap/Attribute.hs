module Rattletrap.Attribute where

import Rattletrap.AttributeValue
import Rattletrap.ClassAttributeMap
import Rattletrap.Map
import Rattletrap.Primitive
import Rattletrap.StreamMap

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeName :: Text
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , attributeValue :: AttributeValue
  } deriving (Eq, Show)

getAttributes
  :: (Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet [Attribute]
getAttributes version classAttributeMap actorMap actorId = do
  hasAttribute <- BinaryBit.getBool
  if not hasAttribute
    then pure []
    else do
      attribute <- getAttribute version classAttributeMap actorMap actorId
      attributes <- getAttributes version classAttributeMap actorMap actorId
      pure (attribute : attributes)

putAttributes :: [Attribute] -> BinaryBit.BitPut ()
putAttributes attributes = do
  mapM_ putAttribute attributes
  BinaryBit.putBool False

getAttribute
  :: (Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet Attribute
getAttribute version classAttributeMap actorMap actorId =
  case getAttributeMap classAttributeMap actorMap actorId of
    Nothing -> fail ("could not get attribute map for " ++ show actorId)
    Just attributeMap ->
      case getAttributeIdLimit attributeMap of
        Nothing ->
          fail ("could not get attribute ID limit for " ++ show actorId)
        Just limit -> do
          id_ <- getCompressedWord limit
          case getAttributeName classAttributeMap attributeMap id_ of
            Nothing -> fail ("could not get attribute name for " ++ show id_)
            Just name -> do
              value <- getAttributeValue version name
              pure (Attribute id_ name value)

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  BinaryBit.putBool True
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
