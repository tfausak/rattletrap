{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute where

import qualified Rattletrap.Type.AttributeValue as AttributeValue
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Attribute = Attribute
  { id :: CompressedWord.CompressedWord
  , name :: Str.Str
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , value :: AttributeValue.AttributeValue
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Attribute jsonOptions)

putAttributes :: [Attribute] -> BitPut ()
putAttributes attributes = case attributes of
  [] -> BinaryBits.putBool False
  [attribute] -> do
    bitPut attribute
    BinaryBits.putBool False
  first : rest -> do
    bitPut first
    putAttributes rest

bitPut :: Attribute -> BitPut ()
bitPut attribute = do
  BinaryBits.putBool True
  CompressedWord.bitPut (Rattletrap.Type.Attribute.id attribute)
  AttributeValue.bitPut (value attribute)

decodeAttributesBits
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> Map CompressedWord.CompressedWord Word32le.Word32le
  -> CompressedWord.CompressedWord
  -> BitGet [Attribute]
decodeAttributesBits version classes actors actor = do
  hasAttribute <- getBool
  if hasAttribute
    then
      (:)
      <$> bitGet version classes actors actor
      <*> decodeAttributesBits version classes actors actor
    else pure []

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> Map CompressedWord.CompressedWord Word32le.Word32le
  -> CompressedWord.CompressedWord
  -> BitGet Attribute
bitGet version classes actors actor = do
  attributes <- lookupAttributeMap classes actors actor
  limit <- lookupAttributeIdLimit attributes actor
  attribute <- CompressedWord.bitGet limit
  name_ <- lookupAttributeName classes attributes attribute
  Attribute attribute name_
    <$> AttributeValue.bitGet
          version
          (ClassAttributeMap.objectMap classes)
          name_

lookupAttributeMap
  :: ClassAttributeMap.ClassAttributeMap
  -> Map CompressedWord.CompressedWord Word32le.Word32le
  -> CompressedWord.CompressedWord
  -> BitGet (Map Word32le.Word32le Word32le.Word32le)
lookupAttributeMap classes actors actor = fromMaybe
  ("[RT01] could not get attribute map for " <> show actor)
  (ClassAttributeMap.getAttributeMap classes actors actor)

lookupAttributeIdLimit
  :: Map Word32le.Word32le Word32le.Word32le -> CompressedWord.CompressedWord -> BitGet Word
lookupAttributeIdLimit attributes actor = fromMaybe
  ("[RT02] could not get attribute ID limit for " <> show actor)
  (ClassAttributeMap.getAttributeIdLimit attributes)

lookupAttributeName
  :: ClassAttributeMap.ClassAttributeMap
  -> Map Word32le.Word32le Word32le.Word32le
  -> CompressedWord.CompressedWord
  -> BitGet Str.Str
lookupAttributeName classes attributes attribute = fromMaybe
  ("[RT03] could not get attribute name for " <> show attribute)
  (ClassAttributeMap.getAttributeName classes attributes attribute)

fromMaybe :: String -> Maybe a -> BitGet a
fromMaybe message = maybe (fail message) pure
