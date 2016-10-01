{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Location where

import Rattletrap.CompressedWord

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified GHC.Generics as Generics

data Location = Location
  { locationBitSize :: CompressedWord
  , locationDx :: CompressedWord
  , locationDy :: CompressedWord
  , locationDz :: CompressedWord
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Location

instance Aeson.ToJSON Location

getLocation :: BinaryBit.BitGet Location
getLocation = do
  bitSize <- getCompressedWord 19
  let limit = 2 ^ (compressedWordValue bitSize + 2)
  dx <- getCompressedWord limit
  dy <- getCompressedWord limit
  dz <- getCompressedWord limit
  pure
    Location
    { locationBitSize = bitSize
    , locationDx = dx
    , locationDy = dy
    , locationDz = dz
    }

putLocation :: Location -> BinaryBit.BitPut ()
putLocation location = do
  putCompressedWord (locationBitSize location)
  putCompressedWord (locationDx location)
  putCompressedWord (locationDy location)
  putCompressedWord (locationDz location)
