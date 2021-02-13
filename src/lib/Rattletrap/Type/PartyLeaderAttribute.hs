{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PartyLeaderAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8le
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8le)
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''PartyLeaderAttribute)

putPartyLeaderAttribute :: PartyLeaderAttribute -> BinaryBits.BitPut ()
putPartyLeaderAttribute partyLeaderAttribute = do
  putWord8Bits (partyLeaderAttributeSystemId partyLeaderAttribute)
  case partyLeaderAttributeId partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      putRemoteId remoteId
      putWord8Bits localId
