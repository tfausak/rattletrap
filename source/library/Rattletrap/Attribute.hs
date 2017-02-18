module Rattletrap.Attribute where

import Rattletrap.AttributeValue
import Rattletrap.Map
import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Vector as Vector

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
  -> BinaryBit.BitGet (Vector.Vector Attribute)
getAttributes version classAttributeMap actorMap actorId =
  let go attributes = do
        hasAttribute <- BinaryBit.getBool
        if hasAttribute
          then do
            attribute <-
              getAttribute version classAttributeMap actorMap actorId
            go (attribute : attributes)
          else pure (Vector.fromList (reverse attributes))
  in go []

putAttributes :: Vector.Vector Attribute -> BinaryBit.BitPut ()
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
