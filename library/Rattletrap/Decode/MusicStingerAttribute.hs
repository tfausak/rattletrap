module Rattletrap.Decode.MusicStingerAttribute
  ( getMusicStingerAttribute
  ) where

import Rattletrap.Type.MusicStingerAttribute
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le

import qualified Data.Binary.Bits.Get as BinaryBit

getMusicStingerAttribute :: BinaryBit.BitGet MusicStingerAttribute
getMusicStingerAttribute = do
  flag <- BinaryBit.getBool
  cue <- getWord32Bits
  trigger <- getWord8Bits
  pure (MusicStingerAttribute flag cue trigger)
