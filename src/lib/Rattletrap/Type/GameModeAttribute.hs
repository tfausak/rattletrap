{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.GameModeAttribute where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data GameModeAttribute = GameModeAttribute
  { gameModeAttributeNumBits :: Int
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Int' rather than something more precise like an
  -- 'Int8' because it just gets passed to functions that expect 'Int's.
  -- There's no reason to do a bunch of conversions.
  , gameModeAttributeWord :: Word8
  }
  deriving (Eq, Show)

$(deriveJson ''GameModeAttribute)

putGameModeAttribute :: GameModeAttribute -> BinaryBits.BitPut ()
putGameModeAttribute gameModeAttribute = do
  let numBits_ = gameModeAttributeNumBits gameModeAttribute
  let word = gameModeAttributeWord gameModeAttribute
  BinaryBits.putWord8 numBits_ word

decodeGameModeAttributeBits :: (Int, Int, Int) -> DecodeBits GameModeAttribute
decodeGameModeAttributeBits version =
  GameModeAttribute (numBits version) <$> getWord8Bits
    (numBits version)

numBits :: (Int, Int, Int) -> Int
numBits version = if version >= (868, 12, 0) then 8 else 2
