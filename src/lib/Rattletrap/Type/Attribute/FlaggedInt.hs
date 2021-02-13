{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedInt where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedIntAttribute = FlaggedIntAttribute
  { flaggedIntAttributeFlag :: Bool
  , flaggedIntAttributeInt :: Int32le
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedIntAttribute)

putFlaggedIntAttribute :: FlaggedIntAttribute -> BinaryBits.BitPut ()
putFlaggedIntAttribute flaggedIntAttribute = do
  BinaryBits.putBool (flaggedIntAttributeFlag flaggedIntAttribute)
  putInt32Bits (flaggedIntAttributeInt flaggedIntAttribute)

decodeFlaggedIntAttributeBits :: BitGet FlaggedIntAttribute
decodeFlaggedIntAttributeBits =
  FlaggedIntAttribute <$> getBool <*> decodeInt32leBits
