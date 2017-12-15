module Rattletrap.Decode.Attribute
  ( getAttributes
  , getAttribute
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Type.Attribute
import Rattletrap.Decode.AttributeValue
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Word32le
import Rattletrap.Decode.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getAttributes
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
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

getAttribute
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le
  -> CompressedWord
  -> BinaryBit.BitGet Attribute
getAttribute version classAttributeMap actorMap actorId =
  case getAttributeMap classAttributeMap actorMap actorId of
    Nothing -> fail ("could not get attribute map for " <> show actorId)
    Just attributeMap -> case getAttributeIdLimit attributeMap of
      Nothing -> fail ("could not get attribute ID limit for " <> show actorId)
      Just limit -> do
        id_ <- getCompressedWord limit
        case getAttributeName classAttributeMap attributeMap id_ of
          Nothing -> fail ("could not get attribute name for " <> show id_)
          Just name -> do
            value <- getAttributeValue
              version
              (classAttributeMapObjectMap classAttributeMap)
              name
            pure (Attribute id_ name value)
