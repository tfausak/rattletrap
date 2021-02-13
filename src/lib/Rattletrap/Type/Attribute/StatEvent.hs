{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.StatEvent where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data StatEventAttribute = StatEventAttribute
  { statEventAttributeUnknown :: Bool
  , statEventAttributeObjectId :: Int32le.Int32le
  }
  deriving (Eq, Show)

$(deriveJson ''StatEventAttribute)

putStatEventAttribute :: StatEventAttribute -> BitPut ()
putStatEventAttribute statEventAttribute = do
  BinaryBits.putBool (statEventAttributeUnknown statEventAttribute)
  Int32le.bitPut (statEventAttributeObjectId statEventAttribute)

decodeStatEventAttributeBits :: BitGet StatEventAttribute
decodeStatEventAttributeBits =
  StatEventAttribute <$> getBool <*> Int32le.bitGet
