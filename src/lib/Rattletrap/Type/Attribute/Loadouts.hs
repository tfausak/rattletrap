{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadouts where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.Loadout
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data LoadoutsAttribute = LoadoutsAttribute
  { loadoutsAttributeBlue :: LoadoutAttribute
  , loadoutsAttributeOrange :: LoadoutAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutsAttribute)

putLoadoutsAttribute :: LoadoutsAttribute -> BinaryBits.BitPut ()
putLoadoutsAttribute loadoutsAttribute = do
  putLoadoutAttribute (loadoutsAttributeBlue loadoutsAttribute)
  putLoadoutAttribute (loadoutsAttributeOrange loadoutsAttribute)

decodeLoadoutsAttributeBits :: DecodeBits LoadoutsAttribute
decodeLoadoutsAttributeBits =
  LoadoutsAttribute
    <$> decodeLoadoutAttributeBits
    <*> decodeLoadoutAttributeBits
