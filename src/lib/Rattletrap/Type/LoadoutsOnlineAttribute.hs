{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LoadoutsOnlineAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.LoadoutOnlineAttribute

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
