module Rattletrap.Encode.StatTitle
  ( putStatTitleBits
  )
where

import Rattletrap.Encode.FlaggedIntAttribute
import Rattletrap.Encode.Str
import Rattletrap.Encode.Word32le
import Rattletrap.Type.StatTitle

import qualified Data.Binary.Bits.Put as BinaryBits

putStatTitleBits :: StatTitle -> BinaryBits.BitPut ()
putStatTitleBits x = do
  BinaryBits.putBool (statTitleUnknown x)
  putTextBits (statTitleName x)
  putFlaggedIntAttribute (statTitleObjectTarget x)
  putWord32Bits (statTitleValue x)
