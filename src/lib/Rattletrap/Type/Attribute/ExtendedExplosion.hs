{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ExtendedExplosion where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.Explosion
import Rattletrap.Type.Attribute.FlaggedInt
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeExplosion :: ExplosionAttribute
  , extendedExplosionAttributeUnknown :: FlaggedIntAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''ExtendedExplosionAttribute)

putExtendedExplosionAttribute
  :: ExtendedExplosionAttribute -> BitPut ()
putExtendedExplosionAttribute x = do
  putExplosionAttribute (extendedExplosionAttributeExplosion x)
  putFlaggedIntAttribute (extendedExplosionAttributeUnknown x)

decodeExtendedExplosionAttributeBits
  :: (Int, Int, Int) -> BitGet ExtendedExplosionAttribute
decodeExtendedExplosionAttributeBits version =
  ExtendedExplosionAttribute
    <$> decodeExplosionAttributeBits version
    <*> decodeFlaggedIntAttributeBits
