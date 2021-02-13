{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PrivateMatchSettingsAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Put as BinaryBits

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Str
  , privateMatchSettingsAttributeJoinableBy :: Word32le
  , privateMatchSettingsAttributeMaxPlayers :: Word32le
  , privateMatchSettingsAttributeGameName :: Str
  , privateMatchSettingsAttributePassword :: Str
  , privateMatchSettingsAttributeFlag :: Bool
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''PrivateMatchSettingsAttribute)

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
