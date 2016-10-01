{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Int8 where

import Rattletrap.Utility

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Int as Int
import qualified GHC.Generics as Generics

newtype Int8 = Int8
  { int8Value :: Int.Int8
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Int8

instance Aeson.ToJSON Int8

getInt8 :: Binary.Get Int8
getInt8 = do
  int8 <- Binary.getInt8
  pure (Int8 int8)

putInt8 :: Int8 -> Binary.Put
putInt8 (Int8 int8) = Binary.putInt8 int8

getInt8Bits :: BinaryBit.BitGet Int8
getInt8Bits = do
  bytes <- BinaryBit.getLazyByteString 1
  pure (Binary.runGet getInt8 (reverseBytes bytes))

putInt8Bits :: Int8 -> BinaryBit.BitPut ()
putInt8Bits int8 = do
  let bytes = Binary.runPut (putInt8 int8)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
