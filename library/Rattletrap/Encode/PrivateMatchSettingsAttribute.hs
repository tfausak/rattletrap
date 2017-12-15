module Rattletrap.Encode.PrivateMatchSettingsAttribute
  ( putPrivateMatchSettingsAttribute
  ) where

import Rattletrap.Type.PrivateMatchSettingsAttribute
import Rattletrap.Encode.Word32le
import Rattletrap.Encode.Str

import qualified Data.Binary.Bits.Put as BinaryBit

putPrivateMatchSettingsAttribute
  :: PrivateMatchSettingsAttribute -> BinaryBit.BitPut ()
putPrivateMatchSettingsAttribute privateMatchSettingsAttribute = do
  putTextBits
    (privateMatchSettingsAttributeMutators privateMatchSettingsAttribute)
  putWord32Bits
    (privateMatchSettingsAttributeJoinableBy privateMatchSettingsAttribute)
  putWord32Bits
    (privateMatchSettingsAttributeMaxPlayers privateMatchSettingsAttribute)
  putTextBits
    (privateMatchSettingsAttributeGameName privateMatchSettingsAttribute)
  putTextBits
    (privateMatchSettingsAttributePassword privateMatchSettingsAttribute)
  BinaryBit.putBool
    (privateMatchSettingsAttributeFlag privateMatchSettingsAttribute)
