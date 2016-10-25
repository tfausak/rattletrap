module Rattletrap.AttributeValue.PartyLeader where

import Rattletrap.RemoteId
import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PartyLeaderAttributeValue = PartyLeaderAttributeValue
  { partyLeaderAttributeValueSystemId :: Word8
  , partyLeaderAttributeValueId :: Maybe (RemoteId, Word8)
  } deriving (Eq, Ord, Show)

getPartyLeaderAttributeValue :: BinaryBit.BitGet PartyLeaderAttributeValue
getPartyLeaderAttributeValue = do
  systemId <- getWord8Bits
  maybeRemoteAndLocalId <-
    if systemId == Word8 0
      then pure Nothing
      else do
        remoteId <- getRemoteId systemId
        localId <- getWord8Bits
        pure (Just (remoteId, localId))
  pure (PartyLeaderAttributeValue systemId maybeRemoteAndLocalId)

putPartyLeaderAttributeValue :: PartyLeaderAttributeValue -> BinaryBit.BitPut ()
putPartyLeaderAttributeValue partyLeaderAttributeValue = do
  putWord8Bits (partyLeaderAttributeValueSystemId partyLeaderAttributeValue)
  case partyLeaderAttributeValueId partyLeaderAttributeValue of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      putRemoteId remoteId
      putWord8Bits localId
