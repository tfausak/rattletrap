module Rattletrap.Attribute.FlaggedInt where

import Rattletrap.Type.Int32
import Rattletrap.Decode.Int32
import Rattletrap.Encode.Int32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data FlaggedIntAttribute = FlaggedIntAttribute
  { flaggedIntAttributeFlag :: Bool
  , flaggedIntAttributeInt :: Int32
  } deriving (Eq, Ord, Show)

getFlaggedIntAttribute :: BinaryBit.BitGet FlaggedIntAttribute
getFlaggedIntAttribute = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttribute flag int)

putFlaggedIntAttribute :: FlaggedIntAttribute -> BinaryBit.BitPut ()
putFlaggedIntAttribute flaggedIntAttribute = do
  BinaryBit.putBool (flaggedIntAttributeFlag flaggedIntAttribute)
  putInt32Bits (flaggedIntAttributeInt flaggedIntAttribute)
