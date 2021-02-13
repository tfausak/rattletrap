{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8Vector where

import Rattletrap.Type.Common
import Rattletrap.Type.Int8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Int8Vector = Int8Vector
  { int8VectorX :: Maybe Int8le
  , int8VectorY :: Maybe Int8le
  , int8VectorZ :: Maybe Int8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Int8Vector)

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

decodeInt8VectorBits :: DecodeBits Int8Vector
decodeInt8VectorBits =
  Int8Vector <$> decodeFieldBits <*> decodeFieldBits <*> decodeFieldBits

decodeFieldBits :: DecodeBits (Maybe Int8le)
decodeFieldBits = do
  hasField <- getBool
  decodeWhen hasField decodeInt8leBits
