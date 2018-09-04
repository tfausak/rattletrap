module Rattletrap.Encode.StatEventAttribute
  ( putStatEventAttribute
  )
where

import Rattletrap.Encode.Int32le
import Rattletrap.Type.StatEventAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putStatEventAttribute :: StatEventAttribute -> BinaryBits.BitPut ()
putStatEventAttribute statEventAttribute = do
  BinaryBits.putBool (statEventAttributeUnknown statEventAttribute)
  putInt32Bits (statEventAttributeObjectId statEventAttribute)
