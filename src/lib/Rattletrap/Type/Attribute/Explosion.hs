{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Explosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ExplosionAttribute = ExplosionAttribute
  { flag :: Bool
  , actorId :: Int32le.Int32le
  , location :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''ExplosionAttribute)

bitPut :: ExplosionAttribute -> BitPut ()
bitPut explosionAttribute = do
  BinaryBits.putBool (flag explosionAttribute)
  Int32le.bitPut (actorId explosionAttribute)
  Vector.bitPut (location explosionAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet ExplosionAttribute
bitGet version =
  ExplosionAttribute
    <$> getBool
    <*> Int32le.bitGet
    <*> Vector.bitGet version
