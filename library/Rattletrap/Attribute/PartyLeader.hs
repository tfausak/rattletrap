module Rattletrap.Attribute.PartyLeader where

import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.Encode.Word8
import Rattletrap.RemoteId

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8)
  } deriving (Eq, Ord, Show)

getPartyLeaderAttribute :: (Int, Int, Int) -> BinaryBit.BitGet PartyLeaderAttribute
getPartyLeaderAttribute version = do
  systemId <- getWord8Bits
  maybeRemoteAndLocalId <- if systemId == Word8 0
    then pure Nothing
    else do
      remoteId <- getRemoteId version systemId
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
