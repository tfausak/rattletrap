module Rattletrap.AttributeValue.FlaggedInt where

import Rattletrap.Int32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data FlaggedIntAttributeValue = FlaggedIntAttributeValue
  { flaggedIntAttributeValueFlag :: Bool
  , flaggedIntAttributeValueInt :: Int32
  } deriving (Eq, Ord, Show)

getFlaggedIntAttributeValue :: BinaryBit.BitGet FlaggedIntAttributeValue
getFlaggedIntAttributeValue = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttributeValue flag int)

putFlaggedIntAttributeValue :: FlaggedIntAttributeValue -> BinaryBit.BitPut ()
putFlaggedIntAttributeValue flaggedIntAttributeValue = do
  BinaryBit.putBool (flaggedIntAttributeValueFlag flaggedIntAttributeValue)
  putInt32Bits (flaggedIntAttributeValueInt flaggedIntAttributeValue)
