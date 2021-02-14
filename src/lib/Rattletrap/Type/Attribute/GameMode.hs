{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.GameMode where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data GameMode = GameMode
  { numBits :: Int
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Int' rather than something more precise like an
  -- 'Int8' because it just gets passed to functions that expect 'Int's.
  -- There's no reason to do a bunch of conversions.
  , word :: Word8
  }
  deriving (Eq, Show)

$(deriveJson ''GameMode)

bitPut :: GameMode -> BitPut ()
bitPut gameModeAttribute = do
  BinaryBits.putWord8 (numBits gameModeAttribute) (word gameModeAttribute)

bitGet :: (Int, Int, Int) -> BitGet GameMode
bitGet version =
  GameMode (numBits_ version) <$> getWord8Bits
    (numBits_ version)

numBits_ :: (Int, Int, Int) -> Int
numBits_ version = if version >= (868, 12, 0) then 8 else 2
