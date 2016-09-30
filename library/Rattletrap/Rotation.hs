module Rattletrap.Rotation where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data Rotation = Rotation
  { rotationX :: Maybe Word.Word8
  , rotationY :: Maybe Word.Word8
  , rotationZ :: Maybe Word.Word8
  } deriving (Eq, Ord, Show)

getRotation :: BinaryBit.BitGet Rotation
getRotation = do
  hasX <- BinaryBit.getBool
  x <-
    if hasX
      then do
        x <- BinaryBit.getWord8 8
        pure (Just x)
      else pure Nothing
  hasY <- BinaryBit.getBool
  y <-
    if hasY
      then do
        y <- BinaryBit.getWord8 8
        pure (Just y)
      else pure Nothing
  hasZ <- BinaryBit.getBool
  z <-
    if hasZ
      then do
        z <- BinaryBit.getWord8 8
        pure (Just z)
      else pure Nothing
  pure Rotation {rotationX = x, rotationY = y, rotationZ = z}

putRotation :: Rotation -> BinaryBit.BitPut ()
putRotation rotation = do
  case rotationX rotation of
    Nothing -> BinaryBit.putBool False
    Just x -> do
      BinaryBit.putBool True
      BinaryBit.putWord8 8 x
  case rotationY rotation of
    Nothing -> BinaryBit.putBool False
    Just y -> do
      BinaryBit.putBool True
      BinaryBit.putWord8 8 y
  case rotationZ rotation of
    Nothing -> BinaryBit.putBool False
    Just z -> do
      BinaryBit.putBool True
      BinaryBit.putWord8 8 z
