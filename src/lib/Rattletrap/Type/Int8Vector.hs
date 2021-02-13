{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8Vector where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int8le as Int8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Int8Vector = Int8Vector
  { x :: Maybe Int8le.Int8le
  , y :: Maybe Int8le.Int8le
  , z :: Maybe Int8le.Int8le
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Int8Vector jsonOptions)

bitPut :: Int8Vector -> BitPut ()
bitPut int8Vector = do
  putInt8VectorField (x int8Vector)
  putInt8VectorField (y int8Vector)
  putInt8VectorField (z int8Vector)

putInt8VectorField :: Maybe Int8le.Int8le -> BitPut ()
putInt8VectorField maybeField = case maybeField of
  Nothing -> BinaryBits.putBool False
  Just field -> do
    BinaryBits.putBool True
    Int8le.bitPut field

bitGet :: BitGet Int8Vector
bitGet =
  Int8Vector <$> decodeFieldBits <*> decodeFieldBits <*> decodeFieldBits

decodeFieldBits :: BitGet (Maybe Int8le.Int8le)
decodeFieldBits = do
  hasField <- getBool
  decodeWhen hasField Int8le.bitGet
