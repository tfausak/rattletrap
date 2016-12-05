module Rattletrap.Attribute.PartyLeader where

import Rattletrap.Primitive
import Rattletrap.RemoteId

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8)
  } deriving (Eq, Ord, Show)

getPartyLeaderAttribute :: BinaryBit.BitGet PartyLeaderAttribute
getPartyLeaderAttribute = do
  systemId <- getWord8Bits
  maybeRemoteAndLocalId <-
    if systemId == Word8 0
      then pure Nothing
      else do
        remoteId <- getRemoteId systemId
        localId <- getWord8Bits
        pure (Just (remoteId, localId))
  pure (PartyLeaderAttribute systemId maybeRemoteAndLocalId)

putPartyLeaderAttribute :: PartyLeaderAttribute -> BinaryBit.BitPut ()
putPartyLeaderAttribute partyLeaderAttribute = do
  putWord8Bits (partyLeaderAttributeSystemId partyLeaderAttribute)
  case partyLeaderAttributeId partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      putRemoteId remoteId
      putWord8Bits localId
