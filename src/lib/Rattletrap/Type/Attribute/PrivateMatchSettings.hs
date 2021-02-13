{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PrivateMatchSettings where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Str.Str
  , privateMatchSettingsAttributeJoinableBy :: Word32le.Word32le
  , privateMatchSettingsAttributeMaxPlayers :: Word32le.Word32le
  , privateMatchSettingsAttributeGameName :: Str.Str
  , privateMatchSettingsAttributePassword :: Str.Str
  , privateMatchSettingsAttributeFlag :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''PrivateMatchSettingsAttribute)

putPrivateMatchSettingsAttribute
  :: PrivateMatchSettingsAttribute -> BitPut ()
putPrivateMatchSettingsAttribute privateMatchSettingsAttribute = do
  Str.bitPut
    (privateMatchSettingsAttributeMutators privateMatchSettingsAttribute)
  Word32le.bitPut
    (privateMatchSettingsAttributeJoinableBy privateMatchSettingsAttribute)
  Word32le.bitPut
    (privateMatchSettingsAttributeMaxPlayers privateMatchSettingsAttribute)
  Str.bitPut
    (privateMatchSettingsAttributeGameName privateMatchSettingsAttribute)
  Str.bitPut
    (privateMatchSettingsAttributePassword privateMatchSettingsAttribute)
  BinaryBits.putBool
    (privateMatchSettingsAttributeFlag privateMatchSettingsAttribute)

decodePrivateMatchSettingsAttributeBits
  :: BitGet PrivateMatchSettingsAttribute
decodePrivateMatchSettingsAttributeBits =
  PrivateMatchSettingsAttribute
    <$> Str.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Str.bitGet
    <*> Str.bitGet
    <*> getBool
