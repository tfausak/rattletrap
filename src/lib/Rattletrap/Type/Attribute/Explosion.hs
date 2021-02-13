{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Explosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeFlag :: Bool
  , explosionAttributeActorId :: Int32le.Int32le
  , explosionAttributeLocation :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''ExplosionAttribute)

putExplosionAttribute :: ExplosionAttribute -> BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBits.putBool False
  Int32le.bitPut (explosionAttributeActorId explosionAttribute)
  Vector.bitPut (explosionAttributeLocation explosionAttribute)

decodeExplosionAttributeBits
  :: (Int, Int, Int) -> BitGet ExplosionAttribute
decodeExplosionAttributeBits version =
  ExplosionAttribute
    <$> getBool
    <*> Int32le.bitGet
    <*> Vector.bitGet version
