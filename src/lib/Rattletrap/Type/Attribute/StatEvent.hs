{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.StatEvent where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data StatEventAttribute = StatEventAttribute
  { unknown :: Bool
  , objectId :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''StatEventAttribute)

bitPut :: StatEventAttribute -> BitPut ()
bitPut statEventAttribute = do
  BinaryBits.putBool (unknown statEventAttribute)
  I32.bitPut (objectId statEventAttribute)

bitGet :: BitGet StatEventAttribute
bitGet =
  StatEventAttribute <$> getBool <*> I32.bitGet
