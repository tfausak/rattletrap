{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Destroyed where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data Destroyed
  = Destroyed
  deriving (Eq, Show)

$(deriveJson ''Destroyed)

bitPut :: Destroyed -> BitPut ()
bitPut _ = pure ()

bitGet :: BitGet Destroyed
bitGet = pure Destroyed
