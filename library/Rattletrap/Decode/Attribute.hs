module Rattletrap.Decode.Attribute
  ( getAttributes
  , getAttribute
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.AttributeValue
import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.Attribute
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Get as BinaryBits

getAttributes
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map CompressedWord Word32le
  -> CompressedWord
  -> DecodeBits [Attribute]
getAttributes version classes actors actor = do
  hasAttribute <- BinaryBits.getBool
  if hasAttribute
    then
      (:)
      <$> getAttribute version classes actors actor
      <*> getAttributes version classes actors actor
    else pure []

getAttribute
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map CompressedWord Word32le
  -> CompressedWord
  -> DecodeBits Attribute
getAttribute version classes actors actor = do
  attributes <- lookupAttributeMap classes actors actor
  limit <- lookupAttributeIdLimit attributes actor
  attribute <- getCompressedWord limit
  name <- lookupAttributeName classes attributes attribute
  Attribute attribute name
    <$> getAttributeValue version (classAttributeMapObjectMap classes) name

lookupAttributeMap
  :: ClassAttributeMap
  -> Map CompressedWord Word32le
  -> CompressedWord
  -> DecodeBits (Map Word32le Word32le)
lookupAttributeMap classes actors actor = fromMaybe
  ("could not get attribute map for " <> show actor)
  (getAttributeMap classes actors actor)

lookupAttributeIdLimit
  :: Map Word32le Word32le -> CompressedWord -> DecodeBits Word
lookupAttributeIdLimit attributes actor = fromMaybe
  ("could not get attribute ID limit for " <> show actor)
  (getAttributeIdLimit attributes)

lookupAttributeName
  :: ClassAttributeMap
  -> Map Word32le Word32le
  -> CompressedWord
  -> DecodeBits Str
lookupAttributeName classes attributes attribute = fromMaybe
  ("could not get attribute name for " <> show attribute)
  (getAttributeName classes attributes attribute)

fromMaybe :: String -> Maybe a -> DecodeBits a
fromMaybe message = maybe (fail message) pure
