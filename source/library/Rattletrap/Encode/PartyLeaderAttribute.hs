module Rattletrap.Encode.PartyLeaderAttribute
  ( putPartyLeaderAttribute
  )
where

import Rattletrap.Encode.RemoteId
import Rattletrap.Encode.Word8le
import Rattletrap.Type.PartyLeaderAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putPartyLeaderAttribute :: PartyLeaderAttribute -> BinaryBits.BitPut ()
putPartyLeaderAttribute partyLeaderAttribute = do
  putWord8Bits (partyLeaderAttributeSystemId partyLeaderAttribute)
  case partyLeaderAttributeId partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      putRemoteId remoteId
      putWord8Bits localId
