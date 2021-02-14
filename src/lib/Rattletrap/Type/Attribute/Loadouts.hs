{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadouts where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

data Loadouts = Loadouts
  { blue :: Loadout.Loadout
  , orange :: Loadout.Loadout
  }
  deriving (Eq, Show)

$(deriveJson ''Loadouts)

bitPut :: Loadouts -> BitPut.BitPut
bitPut loadoutsAttribute = do
  Loadout.bitPut (blue loadoutsAttribute)
  Loadout.bitPut (orange loadoutsAttribute)

bitGet :: BitGet Loadouts
bitGet =
  Loadouts
    <$> Loadout.bitGet
    <*> Loadout.bitGet
