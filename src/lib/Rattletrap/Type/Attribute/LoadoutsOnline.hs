{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutsOnline where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.LoadoutOnline as LoadoutOnline
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Encode.Common

import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data LoadoutsOnline = LoadoutsOnline
  { blue :: LoadoutOnline.LoadoutOnline
  , orange :: LoadoutOnline.LoadoutOnline
  , unknown1 :: Bool
  , unknown2 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutsOnline)

bitPut :: LoadoutsOnline -> BitPut ()
bitPut loadoutsOnlineAttribute = do
  LoadoutOnline.bitPut
    (blue loadoutsOnlineAttribute)
  LoadoutOnline.bitPut
    (orange loadoutsOnlineAttribute)
  BinaryBits.putBool (unknown1 loadoutsOnlineAttribute)
  BinaryBits.putBool (unknown2 loadoutsOnlineAttribute)

bitGet
  :: (Int, Int, Int)
  -> Map.Map U32.U32 Str.Str
  -> BitGet LoadoutsOnline
bitGet version objectMap =
  LoadoutsOnline
    <$> LoadoutOnline.bitGet version objectMap
    <*> LoadoutOnline.bitGet version objectMap
    <*> getBool
    <*> getBool
