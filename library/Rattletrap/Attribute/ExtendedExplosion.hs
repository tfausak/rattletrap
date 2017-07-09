module Rattletrap.Attribute.ExtendedExplosion where

import Rattletrap.Primitive

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeUnknown :: [Word8]
  -- ^ Warning: This field should be considered experimental and could change
  -- at any time!
  } deriving (Eq, Ord, Show)

getExtendedExplosionAttribute :: BinaryBit.BitGet ExtendedExplosionAttribute
getExtendedExplosionAttribute = do
  unknown <- Monad.replicateM 14 getWord8Bits
  pure (ExtendedExplosionAttribute unknown)

putExtendedExplosionAttribute :: ExtendedExplosionAttribute -> BinaryBit.BitPut ()
putExtendedExplosionAttribute extendedExplosionAttribute = do
  mapM_ putWord8Bits (extendedExplosionAttributeUnknown extendedExplosionAttribute)
