module Rattletrap.Encode.PrivateMatchSettingsAttribute
  ( putPrivateMatchSettingsAttribute
  )
where

import Rattletrap.Encode.Str
import Rattletrap.Encode.Word32le
import Rattletrap.Type.PrivateMatchSettingsAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putPrivateMatchSettingsAttribute
  :: PrivateMatchSettingsAttribute -> BinaryBits.BitPut ()
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
  BinaryBits.putBool
    (privateMatchSettingsAttributeFlag privateMatchSettingsAttribute)
