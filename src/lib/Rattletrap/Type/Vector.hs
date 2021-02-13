{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Vector where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBits

data Vector = Vector
  { vectorSize :: CompressedWord
  , vectorBias :: Word
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Word' rather than something more precise like a
  -- 'Word8' because it just gets passed to a functions that expect 'Word's.
  -- There's no reason to do a bunch of conversions.
  , vectorX :: Int
  -- ^ See 'vectorBias'.
  , vectorY :: Int
  -- ^ See 'vectorBias'.
  , vectorZ :: Int
  -- ^ See 'vectorBias'.
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Vector)

putVector :: Vector -> BinaryBits.BitPut ()
putVector vector = do
  let
    bitSize =
      round (logBase (2 :: Float) (fromIntegral (vectorBias vector))) - 1 :: Word
    dx =
      fromIntegral (vectorX vector + fromIntegral (vectorBias vector)) :: Word
    dy =
      fromIntegral (vectorY vector + fromIntegral (vectorBias vector)) :: Word
    dz =
      fromIntegral (vectorZ vector + fromIntegral (vectorBias vector)) :: Word
    limit = 2 ^ (bitSize + 2) :: Word
  putCompressedWord (vectorSize vector)
  putCompressedWord (CompressedWord limit dx)
  putCompressedWord (CompressedWord limit dy)
  putCompressedWord (CompressedWord limit dz)
