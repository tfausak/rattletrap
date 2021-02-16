module Rattletrap.Type.Attribute.PrivateMatchSettings where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32

data PrivateMatchSettings = PrivateMatchSettings
  { mutators :: Str.Str
  , joinableBy :: U32.U32
  , maxPlayers :: U32.U32
  , gameName :: Str.Str
  , password :: Str.Str
  , flag :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''PrivateMatchSettings)

bitPut :: PrivateMatchSettings -> BitPut.BitPut
bitPut privateMatchSettingsAttribute =
  Str.bitPut (mutators privateMatchSettingsAttribute)
    <> U32.bitPut (joinableBy privateMatchSettingsAttribute)
    <> U32.bitPut (maxPlayers privateMatchSettingsAttribute)
    <> Str.bitPut (gameName privateMatchSettingsAttribute)
    <> Str.bitPut (password privateMatchSettingsAttribute)
    <> BitPut.bool (flag privateMatchSettingsAttribute)

bitGet :: BitGet.BitGet PrivateMatchSettings
bitGet =
  PrivateMatchSettings
    <$> Str.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> Str.bitGet
    <*> Str.bitGet
    <*> BitGet.bool
