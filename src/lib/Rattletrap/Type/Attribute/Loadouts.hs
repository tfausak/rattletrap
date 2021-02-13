{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadouts where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.Loadout
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data LoadoutsAttribute = LoadoutsAttribute
  { loadoutsAttributeBlue :: LoadoutAttribute
  , loadoutsAttributeOrange :: LoadoutAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutsAttribute)

putLoadoutsAttribute :: LoadoutsAttribute -> BitPut ()
putLoadoutsAttribute loadoutsAttribute = do
  putLoadoutAttribute (loadoutsAttributeBlue loadoutsAttribute)
  putLoadoutAttribute (loadoutsAttributeOrange loadoutsAttribute)

decodeLoadoutsAttributeBits :: BitGet LoadoutsAttribute
decodeLoadoutsAttributeBits =
  LoadoutsAttribute
    <$> decodeLoadoutAttributeBits
    <*> decodeLoadoutAttributeBits
