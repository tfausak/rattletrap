module Rattletrap.Decode.Int8Vector
  ( getInt8Vector
  ) where

import Rattletrap.Primitive.Int8
import Rattletrap.Type.Int8Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getInt8Vector :: BinaryBit.BitGet Int8Vector
getInt8Vector = do
  x <- getInt8VectorField
  y <- getInt8VectorField
  z <- getInt8VectorField
  pure (Int8Vector x y z)

getInt8VectorField :: BinaryBit.BitGet (Maybe Int8)
getInt8VectorField = do
  hasField <- BinaryBit.getBool
  if hasField
    then do
      field <- getInt8Bits
      pure (Just field)
    else pure Nothing
