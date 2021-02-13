{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.DestroyedReplication where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data DestroyedReplication = DestroyedReplication
  deriving (Eq, Show)

$(deriveJson ''DestroyedReplication)

putDestroyedReplication :: DestroyedReplication -> BinaryBits.BitPut ()
putDestroyedReplication _ = pure ()

decodeDestroyedReplicationBits :: DecodeBits DestroyedReplication
decodeDestroyedReplicationBits = pure DestroyedReplication
