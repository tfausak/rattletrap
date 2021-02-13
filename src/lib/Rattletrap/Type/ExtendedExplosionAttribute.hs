{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ExtendedExplosionAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Type.FlaggedIntAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeExplosion :: ExplosionAttribute
  , extendedExplosionAttributeUnknown :: FlaggedIntAttribute
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''ExtendedExplosionAttribute)

putExtendedExplosionAttribute
  :: ExtendedExplosionAttribute -> BinaryBits.BitPut ()
putExtendedExplosionAttribute x = do
  putExplosionAttribute (extendedExplosionAttributeExplosion x)
  putFlaggedIntAttribute (extendedExplosionAttributeUnknown x)
