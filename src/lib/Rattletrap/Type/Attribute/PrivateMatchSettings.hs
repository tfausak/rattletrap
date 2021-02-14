{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PrivateMatchSettings where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { mutators :: Str.Str
  , joinableBy :: U32.U32
  , maxPlayers :: U32.U32
  , gameName :: Str.Str
  , password :: Str.Str
  , flag :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''PrivateMatchSettingsAttribute)

bitPut
  :: PrivateMatchSettingsAttribute -> BitPut ()
bitPut privateMatchSettingsAttribute = do
  Str.bitPut
    (mutators privateMatchSettingsAttribute)
  U32.bitPut
    (joinableBy privateMatchSettingsAttribute)
  U32.bitPut
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
    <*> U32.bitGet
    <*> U32.bitGet
    <*> Str.bitGet
    <*> Str.bitGet
    <*> getBool
