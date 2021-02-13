{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutsOnlineAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Decode.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data LoadoutsOnlineAttribute = LoadoutsOnlineAttribute
  { loadoutsOnlineAttributeBlue :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeOrange :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeUnknown1 :: Bool
  , loadoutsOnlineAttributeUnknown2 :: Bool
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''LoadoutsOnlineAttribute)

putLoadoutsOnlineAttribute :: LoadoutsOnlineAttribute -> BinaryBits.BitPut ()
putLoadoutsOnlineAttribute loadoutsOnlineAttribute = do
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeBlue loadoutsOnlineAttribute)
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeOrange loadoutsOnlineAttribute)
  BinaryBits.putBool (loadoutsOnlineAttributeUnknown1 loadoutsOnlineAttribute)
  BinaryBits.putBool (loadoutsOnlineAttributeUnknown2 loadoutsOnlineAttribute)

decodeLoadoutsOnlineAttributeBits
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> DecodeBits LoadoutsOnlineAttribute
decodeLoadoutsOnlineAttributeBits version objectMap =
  LoadoutsOnlineAttribute
    <$> decodeLoadoutOnlineAttributeBits version objectMap
    <*> decodeLoadoutOnlineAttributeBits version objectMap
    <*> getBool
    <*> getBool
