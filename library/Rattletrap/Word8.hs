{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Word8 where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

newtype Word8 = Word8
  { word8Value :: Word.Word8
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Word8

instance Aeson.ToJSON Word8

getWord8 :: Binary.Get Word8
getWord8 = do
  word8 <- Binary.getWord8
  pure (Word8 word8)

putWord8 :: Word8 -> Binary.Put
putWord8 (Word8 word8) = Binary.putWord8 word8
