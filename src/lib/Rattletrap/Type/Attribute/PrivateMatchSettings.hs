{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PrivateMatchSettings where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { mutators :: Str.Str
  , joinableBy :: Word32le.Word32le
  , maxPlayers :: Word32le.Word32le
  , gameName :: Str.Str
  , password :: Str.Str
  , flag :: Bool
  }
  deriving (Eq, Show)

$(deriveJsonWith ''PrivateMatchSettingsAttribute jsonOptions)

bitPut
  :: PrivateMatchSettingsAttribute -> BitPut ()
bitPut privateMatchSettingsAttribute = do
  Str.bitPut
    (mutators privateMatchSettingsAttribute)
  Word32le.bitPut
    (joinableBy privateMatchSettingsAttribute)
  Word32le.bitPut
    (maxPlayers privateMatchSettingsAttribute)
  Str.bitPut
    (gameName privateMatchSettingsAttribute)
  Str.bitPut
    (password privateMatchSettingsAttribute)
  BinaryBits.putBool
    (flag privateMatchSettingsAttribute)

bitGet
  :: BitGet PrivateMatchSettingsAttribute
bitGet =
  PrivateMatchSettingsAttribute
    <$> Str.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Str.bitGet
    <*> Str.bitGet
    <*> getBool
