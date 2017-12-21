module Rattletrap.Decode.DestroyedReplication
  ( decodeDestroyedReplicationBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.DestroyedReplication

decodeDestroyedReplicationBits :: DecodeBits DestroyedReplication
decodeDestroyedReplicationBits = pure DestroyedReplication
