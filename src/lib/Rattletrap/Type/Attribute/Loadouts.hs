{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Loadouts where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data LoadoutsAttribute = LoadoutsAttribute
  { blue :: Loadout.LoadoutAttribute
  , orange :: Loadout.LoadoutAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''LoadoutsAttribute)

bitPut :: LoadoutsAttribute -> BitPut ()
bitPut loadoutsAttribute = do
  Loadout.bitPut (blue loadoutsAttribute)
  Loadout.bitPut (orange loadoutsAttribute)

bitGet :: BitGet LoadoutsAttribute
bitGet =
  LoadoutsAttribute
    <$> Loadout.bitGet
    <*> Loadout.bitGet
