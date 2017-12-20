module Rattletrap.Decode.LoadoutsOnlineAttribute
  ( decodeLoadoutsOnlineAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map

decodeLoadoutsOnlineAttributeBits
  :: Reader.ReaderT
       ((Int, Int, Int), Map.Map Word32le Str)
       DecodeBits
       LoadoutsOnlineAttribute
decodeLoadoutsOnlineAttributeBits =
  LoadoutsOnlineAttribute
    <$> decodeLoadoutOnlineAttributeBits
    <*> decodeLoadoutOnlineAttributeBits
    <*> Trans.lift getBool
    <*> Trans.lift getBool
