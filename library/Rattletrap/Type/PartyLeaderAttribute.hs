{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PartyLeaderAttribute
  ( PartyLeaderAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Type.RemoteId

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8le
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8le)
  } deriving (Eq, Ord, Show)

$(deriveJson ''PartyLeaderAttribute)
