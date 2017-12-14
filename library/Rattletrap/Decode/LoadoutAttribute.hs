module Rattletrap.Decode.LoadoutAttribute
  ( getLoadoutAttribute
  ) where

import Rattletrap.Type.LoadoutAttribute
import Rattletrap.Decode.Word32
import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8

import qualified Data.Binary.Bits.Get as BinaryBit

getLoadoutAttribute :: BinaryBit.BitGet LoadoutAttribute
getLoadoutAttribute = do
  version <- getWord8Bits
  body <- getWord32Bits
  decal <- getWord32Bits
  wheels <- getWord32Bits
  rocketTrail <- getWord32Bits
  antenna <- getWord32Bits
  topper <- getWord32Bits
  unknown1 <- getWord32Bits
  unknown2 <- getOptional (version > Word8 10) getWord32Bits
  engineAudio <- getOptional (version >= Word8 16) getWord32Bits
  trail <- getOptional (version >= Word8 16) getWord32Bits
  goalExplosion <- getOptional (version >= Word8 16) getWord32Bits
  banner <- getOptional (version >= Word8 17) getWord32Bits
  pure
    ( LoadoutAttribute
      version
      body
      decal
      wheels
      rocketTrail
      antenna
      topper
      unknown1
      unknown2
      engineAudio
      trail
      goalExplosion
      banner
    )

getOptional :: Bool -> BinaryBit.BitGet a -> BinaryBit.BitGet (Maybe a)
getOptional p f = if p
  then do
    x <- f
    pure (Just x)
  else pure Nothing
