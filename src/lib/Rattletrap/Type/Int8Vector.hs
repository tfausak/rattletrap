{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8Vector where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I8 as I8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Int8Vector = Int8Vector
  { x :: Maybe I8.I8
  , y :: Maybe I8.I8
  , z :: Maybe I8.I8
  }
  deriving (Eq, Show)

$(deriveJson ''Int8Vector)

bitPut :: Int8Vector -> BitPut ()
bitPut int8Vector = do
  putInt8VectorField (x int8Vector)
  putInt8VectorField (y int8Vector)
  putInt8VectorField (z int8Vector)

putInt8VectorField :: Maybe I8.I8 -> BitPut ()
putInt8VectorField maybeField = case maybeField of
  Nothing -> BinaryBits.putBool False
  Just field -> do
    BinaryBits.putBool True
    I8.bitPut field

bitGet :: BitGet Int8Vector
bitGet =
  Int8Vector <$> decodeFieldBits <*> decodeFieldBits <*> decodeFieldBits

decodeFieldBits :: BitGet (Maybe I8.I8)
decodeFieldBits = do
  hasField <- getBool
  decodeWhen hasField I8.bitGet
