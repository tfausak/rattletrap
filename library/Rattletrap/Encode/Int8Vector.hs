module Rattletrap.Encode.Int8Vector
  ( putInt8Vector
  ) where

import Rattletrap.Encode.Int8
import Rattletrap.Type.Int8
import Rattletrap.Type.Int8Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putInt8Vector :: Int8Vector -> BinaryBit.BitPut ()
putInt8Vector int8Vector = do
  putInt8VectorField (int8VectorX int8Vector)
  putInt8VectorField (int8VectorY int8Vector)
  putInt8VectorField (int8VectorZ int8Vector)

putInt8VectorField :: Maybe Int8 -> BinaryBit.BitPut ()
putInt8VectorField maybeField = case maybeField of
  Nothing -> BinaryBit.putBool False
  Just field -> do
    BinaryBit.putBool True
    putInt8Bits field