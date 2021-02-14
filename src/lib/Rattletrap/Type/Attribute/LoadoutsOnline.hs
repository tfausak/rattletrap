{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutsOnline where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.LoadoutOnline as LoadoutOnline
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data LoadoutsOnlineAttribute = LoadoutsOnlineAttribute
  { blue :: LoadoutOnline.LoadoutOnlineAttribute
  , orange :: LoadoutOnline.LoadoutOnlineAttribute
  , unknown1 :: Bool
  , unknown2 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutsOnlineAttribute)

bitPut :: LoadoutsOnlineAttribute -> BitPut ()
bitPut loadoutsOnlineAttribute = do
  LoadoutOnline.bitPut
    (blue loadoutsOnlineAttribute)
  LoadoutOnline.bitPut
    (orange loadoutsOnlineAttribute)
  BinaryBits.putBool (unknown1 loadoutsOnlineAttribute)
  BinaryBits.putBool (unknown2 loadoutsOnlineAttribute)

bitGet
  :: (Int, Int, Int)
  -> Map.Map Word32le.Word32le Str.Str
  -> BitGet LoadoutsOnlineAttribute
bitGet version objectMap =
  LoadoutsOnlineAttribute
    <$> LoadoutOnline.bitGet version objectMap
    <*> LoadoutOnline.bitGet version objectMap
    <*> getBool
    <*> getBool
