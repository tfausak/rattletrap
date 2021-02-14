{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedInt where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedIntAttribute = FlaggedIntAttribute
  { flag :: Bool
  , int :: Int32le.Int32le
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedIntAttribute)

bitPut :: FlaggedIntAttribute -> BitPut ()
bitPut flaggedIntAttribute = do
  BinaryBits.putBool (flag flaggedIntAttribute)
  Int32le.bitPut (int flaggedIntAttribute)

bitGet :: BitGet FlaggedIntAttribute
bitGet =
  FlaggedIntAttribute <$> getBool <*> Int32le.bitGet
