{-# LANGUAGE Strict #-}

module Rattletrap.Primitive.Vector where

import Rattletrap.Primitive.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Vector = Vector
  { vectorBias :: Word
  , vectorX :: Int
  , vectorY :: Int
  , vectorZ :: Int
  } deriving (Eq, Show)

getVector :: BinaryBit.BitGet Vector
getVector = do
  bitSize <- getCompressedWord 19
  let limit = 2 ^ (compressedWordValue bitSize + 2)
  dx <- getCompressedWord limit
  dy <- getCompressedWord limit
  dz <- getCompressedWord limit
  let fromCompressedWord x = fromIntegral (compressedWordValue x)
  let bias = 2 ^ (fromCompressedWord bitSize + 1 :: Word)
  let x = fromCompressedWord dx - fromIntegral bias
  let y = fromCompressedWord dy - fromIntegral bias
  let z = fromCompressedWord dz - fromIntegral bias
  pure (Vector bias x y z)

putVector :: Vector -> BinaryBit.BitPut ()
putVector vector = do
  let bitSize =
        round (logBase (2 :: Float) (fromIntegral (vectorBias vector))) - 1
  putCompressedWord (CompressedWord 19 bitSize)
  let dx = fromIntegral (vectorX vector + fromIntegral (vectorBias vector))
  let dy = fromIntegral (vectorY vector + fromIntegral (vectorBias vector))
  let dz = fromIntegral (vectorZ vector + fromIntegral (vectorBias vector))
  let limit = 2 ^ (bitSize + 2)
  putCompressedWord (CompressedWord limit dx)
  putCompressedWord (CompressedWord limit dy)
  putCompressedWord (CompressedWord limit dz)
