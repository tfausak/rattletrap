{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.StringAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Str
  } deriving (Eq, Ord, Show)

$(deriveJson ''StringAttribute)

putStringAttribute :: StringAttribute -> BinaryBits.BitPut ()
putStringAttribute stringAttribute =
  putTextBits (stringAttributeValue stringAttribute)

decodeStringAttributeBits :: DecodeBits StringAttribute
decodeStringAttributeBits = StringAttribute <$> decodeStrBits
