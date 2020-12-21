module Rattletrap.Decode.LoadoutsOnlineAttribute
  ( decodeLoadoutsOnlineAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Map as Map

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
