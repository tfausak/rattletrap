module Rattletrap.AttributeValue.MusicStinger where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data MusicStingerAttributeValue = MusicStingerAttributeValue
  { musicStingerAttributeValueFlag :: Bool
  , musicStingerAttributeValueCue :: Word32
  , musicStingerAttributeValueTrigger :: Word8
  } deriving (Eq, Ord, Show)

getMusicStingerAttributeValue :: BinaryBit.BitGet MusicStingerAttributeValue
getMusicStingerAttributeValue = do
  flag <- BinaryBit.getBool
  cue <- getWord32Bits
  trigger <- getWord8Bits
  pure (MusicStingerAttributeValue flag cue trigger)

putMusicStingerAttributeValue :: MusicStingerAttributeValue
                              -> BinaryBit.BitPut ()
putMusicStingerAttributeValue musicStingerAttributeValue = do
  BinaryBit.putBool (musicStingerAttributeValueFlag musicStingerAttributeValue)
  putWord32Bits (musicStingerAttributeValueCue musicStingerAttributeValue)
  putWord8Bits (musicStingerAttributeValueTrigger musicStingerAttributeValue)
