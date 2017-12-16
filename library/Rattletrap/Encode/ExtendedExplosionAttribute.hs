module Rattletrap.Encode.ExtendedExplosionAttribute
  ( putExtendedExplosionAttribute
  ) where

import Rattletrap.Encode.ExplosionAttribute
import Rattletrap.Encode.FlaggedIntAttribute
import Rattletrap.Type.ExtendedExplosionAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putExtendedExplosionAttribute
  :: ExtendedExplosionAttribute -> BinaryBit.BitPut ()
putExtendedExplosionAttribute x = do
  putExplosionAttribute (extendedExplosionAttributeExplosion x)
  putFlaggedIntAttribute (extendedExplosionAttributeUnknown x)
