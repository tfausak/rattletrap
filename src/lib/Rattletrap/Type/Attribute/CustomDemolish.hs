{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CustomDemolish where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.Demolish
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data CustomDemolishAttribute = CustomDemolishAttribute
  { customDemolishAttributeFlag :: Bool
  , customDemolishAttributeId :: Int32le.Int32le
  , customDemolishAttributeDemolish :: DemolishAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''CustomDemolishAttribute)

putCustomDemolishAttribute :: CustomDemolishAttribute -> BitPut ()
putCustomDemolishAttribute x = do
  BinaryBits.putBool (customDemolishAttributeFlag x)
  Int32le.bitPut (customDemolishAttributeId x)
  putDemolishAttribute (customDemolishAttributeDemolish x)

decodeCustomDemolishAttributeBits
  :: (Int, Int, Int) -> BitGet CustomDemolishAttribute
decodeCustomDemolishAttributeBits version =
  CustomDemolishAttribute
    <$> getBool
    <*> Int32le.bitGet
    <*> decodeDemolishAttributeBits version
