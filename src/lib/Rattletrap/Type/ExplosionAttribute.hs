{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ExplosionAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeFlag :: Bool
  , explosionAttributeActorId :: Int32le
  , explosionAttributeLocation :: Vector
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''ExplosionAttribute)

putExplosionAttribute :: ExplosionAttribute -> BinaryBits.BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBits.putBool False
  putInt32Bits (explosionAttributeActorId explosionAttribute)
  putVector (explosionAttributeLocation explosionAttribute)

decodeExplosionAttributeBits
  :: (Int, Int, Int) -> DecodeBits ExplosionAttribute
decodeExplosionAttributeBits version =
  ExplosionAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits version
