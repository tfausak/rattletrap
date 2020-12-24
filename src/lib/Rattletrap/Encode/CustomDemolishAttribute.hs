module Rattletrap.Encode.CustomDemolishAttribute
  ( putCustomDemolishAttribute
  )
where

import Rattletrap.Encode.DemolishAttribute
import Rattletrap.Encode.Int32le
import Rattletrap.Type.CustomDemolishAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putCustomDemolishAttribute :: CustomDemolishAttribute -> BinaryBits.BitPut ()
putCustomDemolishAttribute x = do
  BinaryBits.putBool (customDemolishAttributeFlag x)
  putInt32Bits (customDemolishAttributeId x)
  putDemolishAttribute (customDemolishAttributeDemolish x)
