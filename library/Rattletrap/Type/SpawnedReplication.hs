module Rattletrap.Type.SpawnedReplication
  ( SpawnedReplication(..)
  ) where

import Rattletrap.Initialization
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

data SpawnedReplication = SpawnedReplication
  { spawnedReplicationFlag :: Bool
  -- ^ Unclear what this is.
  , spawnedReplicationNameIndex :: Maybe Word32
  , spawnedReplicationName :: Maybe Text
  -- ^ Read-only! Changing a replication's name requires editing the
  -- 'spawnedReplicationNameIndex' and maybe the class attribute map.
  , spawnedReplicationObjectId :: Word32
  , spawnedReplicationObjectName :: Text
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , spawnedReplicationClassName :: Text
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , spawnedReplicationInitialization :: Initialization
  } deriving (Eq, Ord, Show)
