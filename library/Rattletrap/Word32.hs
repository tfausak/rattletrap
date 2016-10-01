{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Word32 where

import Rattletrap.Utility

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

newtype Word32 = Word32
  { word32Value :: Word.Word32
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Word32

instance Aeson.ToJSON Word32

getWord32 :: Binary.Get Word32
getWord32 = do
  word32 <- Binary.getWord32le
  pure (Word32 word32)

putWord32 :: Word32 -> Binary.Put
putWord32 (Word32 word32) = Binary.putWord32le word32

getWord32Bits :: BinaryBit.BitGet Word32
getWord32Bits = do
  bytes <- BinaryBit.getLazyByteString 4
  pure (Binary.runGet getWord32 (reverseBytes bytes))

putWord32Bits :: Word32 -> BinaryBit.BitPut ()
putWord32Bits word32 = do
  let bytes = Binary.runPut (putWord32 word32)
  BinaryBit.putByteString (ByteString.toStrict (reverseBytes bytes))
