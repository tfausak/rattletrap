{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.String where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Str
  } deriving (Eq, Show)

$(deriveJson ''StringAttribute)

putStringAttribute :: StringAttribute -> BinaryBits.BitPut ()
putStringAttribute stringAttribute =
  putTextBits (stringAttributeValue stringAttribute)

decodeStringAttributeBits :: BitGet StringAttribute
decodeStringAttributeBits = StringAttribute <$> decodeStrBits
