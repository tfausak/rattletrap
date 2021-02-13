{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CustomDemolish where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.Demolish
import Rattletrap.Type.Int32le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data CustomDemolishAttribute = CustomDemolishAttribute
  { customDemolishAttributeFlag :: Bool
  , customDemolishAttributeId :: Int32le
  , customDemolishAttributeDemolish :: DemolishAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''CustomDemolishAttribute)

putCustomDemolishAttribute :: CustomDemolishAttribute -> BinaryBits.BitPut ()
putCustomDemolishAttribute x = do
  BinaryBits.putBool (customDemolishAttributeFlag x)
  putInt32Bits (customDemolishAttributeId x)
  putDemolishAttribute (customDemolishAttributeDemolish x)

decodeCustomDemolishAttributeBits
  :: (Int, Int, Int) -> BitGet CustomDemolishAttribute
decodeCustomDemolishAttributeBits version =
  CustomDemolishAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> decodeDemolishAttributeBits version
