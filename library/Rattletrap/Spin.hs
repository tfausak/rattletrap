module Rattletrap.Spin where

import Rattletrap.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Spin = Spin
  { spinX :: CompressedWord
  , spinY :: CompressedWord
  , spinZ :: CompressedWord
  } deriving (Eq, Ord, Show)

getSpin :: BinaryBit.BitGet Spin
getSpin = do
  let limit = 65536
  x <- getCompressedWord limit
  y <- getCompressedWord limit
  z <- getCompressedWord limit
  pure Spin {spinX = x, spinY = y, spinZ = z}

putSpin :: Spin -> BinaryBit.BitPut ()
putSpin spin = do
  putCompressedWord (spinX spin)
  putCompressedWord (spinY spin)
  putCompressedWord (spinZ spin)
