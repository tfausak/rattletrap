{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Explosion where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeFlag :: Bool
  , explosionAttributeActorId :: Int32le
  , explosionAttributeLocation :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''ExplosionAttribute)

putExplosionAttribute :: ExplosionAttribute -> BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBits.putBool False
  putInt32Bits (explosionAttributeActorId explosionAttribute)
  Vector.bitPut (explosionAttributeLocation explosionAttribute)

decodeExplosionAttributeBits
  :: (Int, Int, Int) -> BitGet ExplosionAttribute
decodeExplosionAttributeBits version =
  ExplosionAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> Vector.bitGet version
