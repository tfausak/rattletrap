{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.SpawnedReplication
  ( SpawnedReplication(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Initialization
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data SpawnedReplication = SpawnedReplication
  { spawnedReplicationFlag :: Bool
  -- ^ Unclear what this is.
  , spawnedReplicationNameIndex :: Maybe Word32le
  , spawnedReplicationName :: Maybe Str
  -- ^ Read-only! Changing a replication's name requires editing the
  -- 'spawnedReplicationNameIndex' and maybe the class attribute map.
  , spawnedReplicationObjectId :: Word32le
  , spawnedReplicationObjectName :: Str
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , spawnedReplicationClassName :: Str
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , spawnedReplicationInitialization :: Initialization
  } deriving (Eq, Ord, Show)

$(deriveJson ''SpawnedReplication)
