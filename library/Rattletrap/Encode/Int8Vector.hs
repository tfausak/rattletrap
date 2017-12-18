module Rattletrap.Encode.Int8Vector
  ( putInt8Vector
  ) where

import Rattletrap.Encode.Int8le
import Rattletrap.Type.Int8Vector
import Rattletrap.Type.Int8le

import qualified Data.Binary.Bits.Put as BinaryBits

putInt8Vector :: Int8Vector -> BinaryBits.BitPut ()
putInt8Vector int8Vector = do
  putInt8VectorField (int8VectorX int8Vector)
  putInt8VectorField (int8VectorY int8Vector)
  putInt8VectorField (int8VectorZ int8Vector)

putInt8VectorField :: Maybe Int8le -> BinaryBits.BitPut ()
putInt8VectorField maybeField = case maybeField of
  Nothing -> BinaryBits.putBool False
  Just field -> do
    BinaryBits.putBool True
    putInt8Bits field
