{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.StatEvent where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data StatEventAttribute = StatEventAttribute
  { unknown :: Bool
  , objectId :: Int32le.Int32le
  }
  deriving (Eq, Show)

$(deriveJson ''StatEventAttribute)

bitPut :: StatEventAttribute -> BitPut ()
bitPut statEventAttribute = do
  BinaryBits.putBool (unknown statEventAttribute)
  Int32le.bitPut (objectId statEventAttribute)

bitGet :: BitGet StatEventAttribute
bitGet =
  StatEventAttribute <$> getBool <*> Int32le.bitGet
