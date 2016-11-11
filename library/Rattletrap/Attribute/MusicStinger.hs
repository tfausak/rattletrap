module Rattletrap.Attribute.MusicStinger where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data MusicStingerAttribute = MusicStingerAttribute
  { musicStingerAttributeFlag :: Bool
  , musicStingerAttributeCue :: Word32
  , musicStingerAttributeTrigger :: Word8
  } deriving (Eq, Ord, Show)

getMusicStingerAttribute :: BinaryBit.BitGet MusicStingerAttribute
getMusicStingerAttribute = do
  flag <- BinaryBit.getBool
  cue <- getWord32Bits
  trigger <- getWord8Bits
  pure (MusicStingerAttribute flag cue trigger)

putMusicStingerAttribute :: MusicStingerAttribute
                              -> BinaryBit.BitPut ()
putMusicStingerAttribute musicStingerAttribute = do
  BinaryBit.putBool (musicStingerAttributeFlag musicStingerAttribute)
  putWord32Bits (musicStingerAttributeCue musicStingerAttribute)
  putWord8Bits (musicStingerAttributeTrigger musicStingerAttribute)
