{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedInt where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedIntAttribute = FlaggedIntAttribute
  { flaggedIntAttributeFlag :: Bool
  , flaggedIntAttributeInt :: Int32le.Int32le
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedIntAttribute)

putFlaggedIntAttribute :: FlaggedIntAttribute -> BitPut ()
putFlaggedIntAttribute flaggedIntAttribute = do
  BinaryBits.putBool (flaggedIntAttributeFlag flaggedIntAttribute)
  Int32le.bitPut (flaggedIntAttributeInt flaggedIntAttribute)

decodeFlaggedIntAttributeBits :: BitGet FlaggedIntAttribute
decodeFlaggedIntAttributeBits =
  FlaggedIntAttribute <$> getBool <*> Int32le.bitGet
