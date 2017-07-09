module Rattletrap.Attribute.ExtendedExplosion where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  {
  } deriving (Eq, Ord, Show)

getExtendedExplosionAttribute :: BinaryBit.BitGet ExtendedExplosionAttribute
getExtendedExplosionAttribute = do
  -- TODO
  pure ExtendedExplosionAttribute

putExtendedExplosionAttribute :: ExtendedExplosionAttribute -> BinaryBit.BitPut ()
putExtendedExplosionAttribute _ = do
  -- TODO
  pure ()
