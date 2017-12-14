{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.PartyLeaderAttribute
  ( PartyLeaderAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8
import Rattletrap.Type.RemoteId

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8)
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON PartyLeaderAttribute where
  parseJSON = defaultParseJson "PartyLeaderAttribute"

instance ToJSON PartyLeaderAttribute where
  toEncoding = defaultToEncoding "PartyLeaderAttribute"
  toJSON = defaultToJson "PartyLeaderAttribute"
