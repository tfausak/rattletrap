{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutsOnline where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.LoadoutOnline
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data LoadoutsOnlineAttribute = LoadoutsOnlineAttribute
  { loadoutsOnlineAttributeBlue :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeOrange :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeUnknown1 :: Bool
  , loadoutsOnlineAttributeUnknown2 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutsOnlineAttribute)

putLoadoutsOnlineAttribute :: LoadoutsOnlineAttribute -> BitPut ()
putLoadoutsOnlineAttribute loadoutsOnlineAttribute = do
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeBlue loadoutsOnlineAttribute)
  putLoadoutOnlineAttribute
    (loadoutsOnlineAttributeOrange loadoutsOnlineAttribute)
  BinaryBits.putBool (loadoutsOnlineAttributeUnknown1 loadoutsOnlineAttribute)
  BinaryBits.putBool (loadoutsOnlineAttributeUnknown2 loadoutsOnlineAttribute)

decodeLoadoutsOnlineAttributeBits
  :: (Int, Int, Int)
  -> Map.Map Word32le.Word32le Str.Str
  -> BitGet LoadoutsOnlineAttribute
decodeLoadoutsOnlineAttributeBits version objectMap =
  LoadoutsOnlineAttribute
    <$> decodeLoadoutOnlineAttributeBits version objectMap
    <*> decodeLoadoutOnlineAttributeBits version objectMap
    <*> getBool
    <*> getBool
