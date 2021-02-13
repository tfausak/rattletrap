{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ExtendedExplosion where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.Explosion
import Rattletrap.Type.Attribute.FlaggedInt
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeExplosion :: ExplosionAttribute
  , extendedExplosionAttributeUnknown :: FlaggedIntAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''ExtendedExplosionAttribute)

putExtendedExplosionAttribute
  :: ExtendedExplosionAttribute -> BinaryBits.BitPut ()
putExtendedExplosionAttribute x = do
  putExplosionAttribute (extendedExplosionAttributeExplosion x)
  putFlaggedIntAttribute (extendedExplosionAttributeUnknown x)

decodeExtendedExplosionAttributeBits
  :: (Int, Int, Int) -> DecodeBits ExtendedExplosionAttribute
decodeExtendedExplosionAttributeBits version =
  ExtendedExplosionAttribute
    <$> decodeExplosionAttributeBits version
    <*> decodeFlaggedIntAttributeBits
