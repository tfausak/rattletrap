{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedInt where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedInt = FlaggedInt
  { flag :: Bool
  , int :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedInt)

bitPut :: FlaggedInt -> BitPut ()
bitPut flaggedIntAttribute = do
  BinaryBits.putBool (flag flaggedIntAttribute)
  I32.bitPut (int flaggedIntAttribute)

bitGet :: BitGet FlaggedInt
bitGet =
  FlaggedInt <$> getBool <*> I32.bitGet
