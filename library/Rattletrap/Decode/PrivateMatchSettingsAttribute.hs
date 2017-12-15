module Rattletrap.Decode.PrivateMatchSettingsAttribute
  ( getPrivateMatchSettingsAttribute
  ) where

import Rattletrap.Type.PrivateMatchSettingsAttribute
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Str

import qualified Data.Binary.Bits.Get as BinaryBit

getPrivateMatchSettingsAttribute
  :: BinaryBit.BitGet PrivateMatchSettingsAttribute
getPrivateMatchSettingsAttribute = do
  mutators <- getTextBits
  joinableBy <- getWord32Bits
  maxPlayers <- getWord32Bits
  gameName <- getTextBits
  password <- getTextBits
  flag <- BinaryBit.getBool
  pure
    ( PrivateMatchSettingsAttribute
      mutators
      joinableBy
      maxPlayers
      gameName
      password
      flag
    )
