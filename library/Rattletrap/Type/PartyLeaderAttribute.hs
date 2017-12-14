module Rattletrap.Type.PartyLeaderAttribute
  ( PartyLeaderAttribute(..)
  ) where

import Rattletrap.Type.Word8
import Rattletrap.RemoteId

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8)
  } deriving (Eq, Ord, Show)
