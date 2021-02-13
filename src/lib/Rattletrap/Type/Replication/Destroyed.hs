{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Destroyed where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data DestroyedReplication = DestroyedReplication
  deriving (Eq, Show)

$(deriveJson ''DestroyedReplication)

putDestroyedReplication :: DestroyedReplication -> BitPut ()
putDestroyedReplication _ = pure ()

decodeDestroyedReplicationBits :: BitGet DestroyedReplication
decodeDestroyedReplicationBits = pure DestroyedReplication
