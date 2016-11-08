module Rattletrap.Int8Vector where

import Rattletrap.Int8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Int8Vector = Int8Vector
  { int8VectorX :: Maybe Int8
  , int8VectorY :: Maybe Int8
  , int8VectorZ :: Maybe Int8
  } deriving (Eq, Ord, Show)

getInt8Vector :: BinaryBit.BitGet Int8Vector
getInt8Vector = do
  x <- getInt8VectorField
  y <- getInt8VectorField
  z <- getInt8VectorField
  pure (Int8Vector x y z)

putInt8Vector :: Int8Vector -> BinaryBit.BitPut ()
putInt8Vector int8Vector = do
  putInt8VectorField (int8VectorX int8Vector)
  putInt8VectorField (int8VectorY int8Vector)
  putInt8VectorField (int8VectorZ int8Vector)

getInt8VectorField :: BinaryBit.BitGet (Maybe Int8)
getInt8VectorField = do
  hasField <- BinaryBit.getBool
  if hasField
    then do
      field <- getInt8Bits
      pure (Just field)
    else pure Nothing

putInt8VectorField :: Maybe Int8 -> BinaryBit.BitPut ()
putInt8VectorField maybeField =
  case maybeField of
    Nothing -> BinaryBit.putBool False
    Just field -> do
      BinaryBit.putBool True
      putInt8Bits field
