{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Explosion where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeFlag :: Bool
  , explosionAttributeActorId :: Int32le
  , explosionAttributeLocation :: Vector
  }
  deriving (Eq, Show)

$(deriveJson ''ExplosionAttribute)

putExplosionAttribute :: ExplosionAttribute -> BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBits.putBool False
  putInt32Bits (explosionAttributeActorId explosionAttribute)
  putVector (explosionAttributeLocation explosionAttribute)

decodeExplosionAttributeBits
  :: (Int, Int, Int) -> BitGet ExplosionAttribute
decodeExplosionAttributeBits version =
  ExplosionAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits version
